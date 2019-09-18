library(tidyverse)
library(tidytext)
library(plotly)
library(DT)
library(tm)
library(data.table)
library(scales)
library(wordcloud2)
library(gridExtra)
library(ngram)
library(shiny) 
library(RColorBrewer)
library(sentimentr)
library(syuzhet)

# setwd('C:/wwyws/Documents/Columbia/STAT5243 Applied Data Science/fall2019-proj1--wwyws0000/doc')
setwd('C:/Users/wwyws/Documents/Columbia/STAT5243 Applied Data Science/fall2019-proj1--wwyws0000/doc')

# load lyrics data
load('../output/processed_lyrics.RData') 
# load artist information
dt_artist <- fread('../data/artists.csv') 

lyrics_list <- c("Folk", "R&B", "Electronic", "Jazz", "Indie", "Country", "Rock", "Metal", "Pop", "Hip-Hop", "Other")
time_list <- c("1970s", "1980s", "1990s", "2000s", "2010s")
corpus <- VCorpus(VectorSource(dt_lyrics$stemmedwords))
word_tibble <- tidy(corpus) %>%
  select(text) %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text)



years <- seq(1970,2010,by = 10)
dt_lyrics <- dt_lyrics[dt_lyrics$year>=1970,]
# dt_lyrics <- cbind(dt_lyrics,decade = years[findInterval(dt_lyrics$year,years)],songLen = nchar(dt_lyrics$lyrics))
dt_lyrics <- cbind(dt_lyrics,decade = years[findInterval(dt_lyrics$year,years)],songLen = str_count(dt_lyrics$lyrics,pattern = '\\w+'))



##bigrams
lyric_bigrams <- dt_lyrics %>%
  unnest_tokens(bigram, stemmedwords, token = "ngrams", n = 2)

bigram_counts <- lyric_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2,decade, sort = TRUE)

plotList <- list()
for (i in 1:length(years)){
  bigram_counts_sub <- bigram_counts[bigram_counts$decade==years[i],]
  combined_words <- apply(bigram_counts_sub[c(1, 2)], 1, paste , collapse = " " )[1:10]
  x_names <- factor(combined_words, levels = rev(combined_words))
  plotList[[i]] <- plot_ly(
    x = bigram_counts_sub$n[1:10],
    y = x_names,
    name =years[i],
    type = "bar",
    orientation = 'h'
  ) 
  
}

subplot(plotList,nrows = 2)



##proportion of each genre within each decade

genreCount <- dt_lyrics %>% count(genre,decade, sort = TRUE)
genreCountList <- split(genreCount,genreCount$decade)

plot_ly() %>%
  add_pie(data = genreCountList[[1]], labels = ~genre, values = ~n,
          name = names(genreCountList)[1], domain = list(row = 0, column = 0)) %>%
  add_pie(data = genreCountList[[2]], labels = ~genre, values = ~n,
          name = names(genreCountList)[2], domain = list(row = 0, column = 1)) %>%
  add_pie(data = genreCountList[[3]], labels = ~genre, values = ~n,
          name = names(genreCountList)[3], domain = list(row = 1, column = 0)) %>%
  add_pie(data = genreCountList[[4]], labels = ~genre, values = ~n,
          name = names(genreCountList)[4], domain = list(row = 1, column = 1)) %>%
  add_pie(data = genreCountList[[5]], labels = ~genre, values = ~n,
          name = names(genreCountList)[5], domain = list(row = 1, column = 2)) %>%
  layout(grid=list(rows=2, columns=3),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

##length of songs lyric segments
lyric_segment <- dt_lyrics %>% unnest_tokens(sentence,lyrics,token = 'regex',pattern = '[:punct:]') %>% mutate(segLength = nchar(sentence))
lyrics_segment_len <- lyric_segment %>% select(id,decade,segLength) %>% group_by(id,decade) %>% summarize(aveLength = mean(segLength))
mycolors <- brewer.pal(5,'Accent')

lyrics_segment_len <- do.call(rbind, lapply(split(lyrics_segment_len, f = lyrics_segment_len$decade), FUN = function(x){
   qs <- quantile(x$aveLength,probs = c(0.005,0.995))
   x[x$aveLength >= qs[1] & x$aveLength <= qs[2],]
 }))

lyrics_segment_len$decade <- as.character(lyrics_segment_len$decade)
plot_ly(lyrics_segment_len,y = ~aveLength,color = ~decade,type = 'box',colors = mycolors) %>% layout(yaxis = list(title = 'Average Length of Lyric Segments'))

## length of songs
song_len <- dt_lyrics %>% select(id,decade,songLen)
mycolors <- brewer.pal(5,'Set1')

song_len <- do.call(rbind, lapply(split(song_len, f = song_len$decade), FUN = function(x){
  qs <- quantile(x$songLen,probs = c(0.005,0.995))
  x[x$songLen >= qs[1] & x$songLen <= qs[2],]
}))
song_len$decade <- as.character(song_len$decade)
plot_ly(song_len,y = ~songLen,color = ~decade,type = 'box') %>% layout(yaxis = list(title = 'Average Length(# of words) of Song'))


###sentiment
##since this next segment takes a very long time to run, I've saved the data in the output folder
##for faster loading
dt_lyrics_sent <- get_nrc_sentiment(dt_lyrics$lyrics)
save(dt_lyrics_sent, file="../output/processed_song_sent.RData")
###
# load("../output/processed_song_sent.RData")
dt_lyrics_sent <- (1/dt_lyrics$songLen * dt_lyrics_sent) %>% mutate(label = paste(dt_lyrics$genre,dt_lyrics$decade))
sentMat <-tbl_df(dt_lyrics_sent)%>%
  group_by(label)%>%
  summarise(
    anger=mean(anger,na.rm=TRUE),
    anticipation=mean(anticipation,na.rm=TRUE),
    disgust=mean(disgust,na.rm=TRUE),
    fear=mean(fear,na.rm=TRUE),
    joy=mean(joy,na.rm=TRUE),
    sadness=mean(sadness,na.rm=TRUE),
    surprise=mean(surprise,na.rm=TRUE),
    trust=mean(trust,na.rm=TRUE),
    negative=mean(negative,na.rm=TRUE),
    positive=mean(positive,na.rm=TRUE),
  ) %>% filter(!grepl('Not Available',label))
rnms <- sentMat$label;sentMat <- sentMat[,-1] %>% data.matrix();
rownames(sentMat) <- rnms


d3heatmap(sentMat, scale="none", colors= "Blues",
          xaxis_font_size = 8,Rowv = FALSE,Colv=FALSE,show_grid=TRUE)
