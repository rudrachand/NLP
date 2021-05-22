#Text Mining 
#Importing Library
library(tm)     # for text mining
library(readxl) #Read both xls and xlsx files
library(dplyr)  #dataframe manupulation
library(wordcloud) # word cloud
library(wordcloud2) #word cloud
#Sentiment Analysis Packages
library(syuzhet)
library(lubridate)
library(lubridate)
library(scales)
library(reshape2)
library(ggplot2)
#set working directory
setwd("F:/Text Mining")
#Importing data 
df  <- read_excel(path = "PS_excelFile - 2019-05-15T152502.046.xlsx",
                  sheet = "PS_excelFile - 2019-05-15T15250",col_names = TRUE)
#df  <- df[1:20,]
#Building Corpus
corpus <- iconv(df$Description,to="utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:2])

#Data Cleaning
corpus    <- tm_map(corpus,tolower)   # makes all lower-case
corpus    <- tm_map(corpus,removePunctuation) # remove all punctuations
corpus    <- tm_map(corpus,removeNumbers) # remove all numbers
cleanset  <- tm_map(corpus,removeWords,stopwords('english')) # remove stop words
cleanset  <- tm_map(corpus,removeWords,c('technician','using','and','but','for',
                                         'was','the','they','were','into','had',
                                         'did','his','with','about','from',
                                         'when','back','him','not','tech','while','after','off','that'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x) # removes URLS
cleanset  <- tm_map(cleanset,content_transformer(removeURL))
#cleanset  <- tm_map(cleanset, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
#cleanset  <- tm_map(cleanset,gsub,pattern = 'tech',replacement = 'technician') 
cleanset  <- tm_map(cleanset,stripWhitespace)

#Term Document Matrix
tdm       <- TermDocumentMatrix(cleanset)
tdm
tdm       <- as.matrix(tdm)      
tdm[1:10,1:20]

#Bar Plot
w <- rowSums(tdm)
w <- subset(w,w>20)
w
barplot(w,
        las=2,
        col = rainbow(50))
#wordcloud
w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
# wordcloud(words = names(w),
#           freq = w,
#           max.words = 400,
#           random.order = F,
#           min.freq = 5,colors = brewer.pal(8,'Dark2'),
#           scale = c(5,0.3),
#           rot.per = 0.3)
wordcloud(words = names(w),
          freq = w,
          max.words = 300,
          random.order = F,
          min.freq = 10,colors = brewer.pal(8,'Dark2'),
          rot.per = 0.5)

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
head(w)
write.csv(w, file = "Frequency_word.csv",row.names=FALSE)

#Sentiment Analysis
#Obtain Sentiment Scores
corp <- iconv(df$Description,to="utf-8")
sentiment_score <- get_nrc_sentiment(corp)
head(sentiment_score)
#corp[2]
#get_nrc_sentiment('hit')
#Bar Plot for Sentiment Analysis
barplot(colSums(sentiment_score),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores')
