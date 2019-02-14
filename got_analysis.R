# devtools::install_github("MangoTheCat/GoTr")
# install.packages("repurrrsive")

library(magrittr)
library(tidyverse)
library(httr)
# library(tidytext)
# library(wordcloud2)
# library(ggridges)

# A loop to get paginated responses from ASOIAF API
page <- 1
newpage <- httr::GET(paste0("https://www.anapioficeandfire.com/api/characters?page=", page,"&pageSize=50"), accept_json())
got_content <- newpage %>% content()

while(headers(newpage)$link %>% grepl("rel=\"next\"", .))
  # Collect data until `next` link disappear
{
  page <- page + 1
  newpage <-
    httr::GET(paste0("https://www.anapioficeandfire.com/api/characters?page=",page,"&pageSize=50"),
      accept_json()
    )
  got_content %<>% append(content(newpage))
  print(paste0("Collecting characters:", page * 50 - 50, " - ", page * 50))
}

# Put list data into nested data frame
got_chars_df <-
  got_content %>%
  tibble( # Create an analyzable data frame from the complex list object
    url = map_chr(., "url"),
    name = map_chr(., "name"),
    gender = map_chr(., "gender"),
    culture = map_chr(., "culture"),
    born = map_chr(., "born"),
    died = map_chr(., "died"),
    titles = map(., "titles"),
    aliases = map(., "aliases"),
    father = map_chr(., "father"),
    mother = map_chr(., "mother"),
    spouse = map_chr(., "spouse"),
    allegiances = map(., "allegiances"),
    books = map(., "books"),
    povBooks = map(., "povBooks"),
    tvSeries = map(., "tvSeries"),
    playedBy = map(., "playedBy")
  ) %>% 
  mutate_if(is.character, 
            funs(if_else(trimws(.) == "",NA_character_, .))) %>% # Explicit NA
  rowwise() %>% 
  mutate(
         alias_length = length(aliases), # How many aliases does the person have
         alias_names = unlist(aliases) %>% paste(collapse = ", "), # Collapse aliases
         has_spouse = if_else(!is.na(spouse), 1L, 0L), # Have a spouse?
         birth_year = str_extract(born, " \\d* ") %>% as.numeric(), # Extract birthyear
         death_year = str_extract(died, " \\d* ") %>% as.numeric(), # Extract deathyear
         is_alive = if_else(condition = (is.na(died) & ((300 - birth_year) < 100)), 
                            true = 1L, false = 0L), # Alive?
         age = if_else(as.logical(is_alive), 300 - birth_year, death_year - birth_year)) %>% # Age
  arrange(name)

# Preparing data for analysis on aliases
long_names <-
  got_chars_df %>% 
    select(name, alias_length, alias_names) %>%
    unnest_tokens(word, alias_names) %>% # Create a word-wise tidy dataset
    left_join(get_sentiments("bing"), by = "word") %>% # Add a sentiment dictionary
    rowwise() %>% 
    filter(!grepl(word, str_to_lower(name))) # Remove oiginal names from the alias

# Who has the most aliases? (showing the ones (with at least 5 aliases)
long_names %>% 
  filter(alias_length >= 5) %>% 
  ggplot() +
    aes(y = alias_length, x = fct_reorder(name, alias_length)) +
    geom_col(position = "dodge") +
    scale_x_discrete() +
    coord_flip() +
    labs(y = "Number of aliases", x = "Name")
  
# Show the character alias sentiments
long_names %>% 
  drop_na(sentiment) %>% 
  group_by(name, alias_length, sentiment) %>% 
  count() %>% 
  filter(alias_length > 3) %>% 
  ggplot() +
    aes(y = n, x = fct_reorder(name, n), fill = sentiment) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(y = "Number of aliases", x = "Name")


# Create a wordcloud of the aliases
long_names %>% 
  anti_join(stop_words) %>% # Remove stopwords
  group_by(word) %>% 
  count() %>% 
  wordcloud2()


# Merriage survival -------------------------------------------------------
library(survminer)
library(broom)
library(survival)
library(scales)

spouse_model_1 <-
    got_chars_df %>% 
    glm(is_alive ~ age + has_spouse, family = "binomial", data = .) 


spouse_model_2 <-
    got_chars_df %>% 
    glm(is_alive ~ age + has_spouse + gender, family = "binomial", data = .) 

anova(spouse_model_1, spouse_model_2, test = "Chisq")

MASS::dropterm(spouse_model_2)
temp <-
    got_chars_df %>% 
    select(name, gender, age, has_spouse, birth_year, died, death_year, is_alive)

broom::tidy(survival_model, 
            exponentiate = TRUE, 
            conf.int = TRUE)

summary(survival_model)

survival_model <- survfit(Surv(age, is_alive) ~ has_spouse, data = got_chars_df)
summary(survival_model)

spouse_plot <-
    ggsurvplot(fit = survival_model, 
               data = got_chars_df,
               surv.scale = "percent",
               xlab = "Age (years)",
               conf.int = TRUE,
               conf.int.style = "step",
               censor = FALSE,
               legend.title = "Has spouse",
               legend = c(.9,.9),
               # pval = TRUE,
               risk.table = TRUE,
               ggtheme = theme_grey()
               )

sex_facet_plot <-
    spouse_plot$plot + facet_grid(.~gender)


sex_facet_plot

# Age of dead characters
got_chars_df %>% 
  filter(!is_alive) %>% 
  ggplot() +
    aes(y = age, x = gender, fill = gender) +
    geom_boxplot() +
    geom_jitter()

got_chars_df %>% 
  filter(!is_alive) %>% 
  ggplot() +
    aes(x = age, y = gender, fill = gender) +
    geom_ridg(alpha = .7)

time_of_death_model <-
  got_chars_df %>% 
    filter(!is_alive) %>% 
    lm(age ~ gender * has_spouse, data = .) 

time_of_death_model %>% summary



got_chars_df %>% 
    drop_na(is_alive) %>% 
  # filter(birth_year >=198) %>% # Restrict analysis to recent events in got (last 102 years)
  ggplot() +
  aes(y = age, x = gender, fill = gender) +
  geom_boxplot() +
  facet_grid(is_alive~has_spouse, labeller = label_both)

got_chars_df %>% 
  filter(birth_year >=198) %>% 
  ggplot() +
    aes(x = age, y = is_alive, fill = gender) +
    # geom_jitter() +
    geom_joy() +
    facet_grid(gender~has_spouse, labeller = label_both)


long_names %>% 
  anti_join(stop_words, by = "word") %>% 
  drop_na(sentiment) %>% 
  group_by(name, sentiment) %>% 
  summarise(full_alias = paste(word, collapse = " "),
            alias_length = first(alias_length),
            sentiment_n = n()) %>% 
  filter(sentiment_n>1) %>% 
  arrange(-alias_length) %>% 
  ggplot() +
    aes(x = fct_reorder(name, sentiment_n), y = sentiment_n, fill = sentiment, label = full_alias) +
    geom_col() +
    geom_text(hjust = 1) +
    coord_flip()

long_names
  
survival_model
library(survival)
coxph(Surv(time=rep(1,100), event=y) ~ x)

exp(coef(survival_model)) 

survival_model %>% 
    broom::augment(type.predict = "response") %>% 
    mutate(.fitted = .fitted %>% pmax(0)) %>% 
    ggplot() +
    aes(x = age, y = .fitted, group = has_spouse, ymin = .fitted - .se.fit, ymax = .fitted + .se.fit) +
    geom_ribbon(fill = "grey", alpha = .3) +
    facet_wrap(~gender) +
    geom_step(aes(color = has_spouse))

