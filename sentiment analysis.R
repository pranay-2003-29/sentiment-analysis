install.packages("tidytext")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tm")
install.packages("wordcloud")
install.packages("textdata")

library(tidytext)
library(dplyr)
library(ggplot2)
library(readr)
library(tm)
library(wordcloud)
library(textdata)

# Custom customer review dataset
customer_reviews <- data.frame(
  reviews = c(
    "I absolutely love this product! It works great.",
    "Terrible quality. The product broke after one use.",
    "Good value for the price. Satisfied with the purchase.",
    "Horrible experience. Customer service was unhelpful.",
    "Amazing quality and fast delivery. Highly recommend!",
    "Not worth the money. I'm very disappointed.",
    "The product is decent but could be better.",
    "Excellent service and great product overall!"
  )
)

# Preprocess the text data
reviews_clean <- customer_reviews %>%
  mutate(reviews = tolower(reviews)) %>% # Convert to lowercase
  mutate(reviews = removePunctuation(reviews)) %>% # Remove punctuation
  mutate(reviews = removeNumbers(reviews)) %>% # Remove numbers
  mutate(reviews = stripWhitespace(reviews)) # Remove extra whitespace

# View cleaned data
head(reviews_clean)

# Tokenize the text and join with sentiment lexicon
reviews_sentiment <- reviews_clean %>%
  unnest_tokens(word, reviews) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE)

# View sentiment results
head(reviews_sentiment)

# Generate a word cloud for the reviews
wordcloud(
  words = reviews_sentiment$word,
  freq = reviews_sentiment$n,
  min.freq = 1,
  max.words = 100,
  colors = brewer.pal(8, "Dark2"),
  scale = c(3, 0.5)
)

# Summarize positive and negative sentiments
sentiment_summary <- reviews_sentiment %>%
  group_by(sentiment) %>%
  summarize(total = sum(n))

# Bar plot of sentiments
ggplot(sentiment_summary, aes(x = sentiment, y = total, fill = sentiment)) +
  geom_col() +
  labs(title = "Customer Review Sentiment Analysis", x = "Sentiment", y = "Count") +
  theme_minimal()

# Calculate sentiment scores for each review
review_scores <- reviews_clean %>%
  mutate(id = row_number()) %>% # Add review ID
  unnest_tokens(word, reviews) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(id, sentiment) %>%
  tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

# View sentiment scores
print(review_scores)


write_csv(review_scores, "customer_review_sentiment_scores.csv")
