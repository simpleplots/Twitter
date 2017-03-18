#install.packages("twitteR")
library(twitteR)
library(tidyverse)
library(ggplot2)

#away analysis

away <- getUser("away")
away_followers <- away$getFollowers()
away_follower_data<- data.frame(Username = character(),
                                Follower_count = numeric())

#interate followers and rbind into data_frame created above
for (i in 1:length(away_followers)) {
    Username <- away_followers[[i]]$getScreenName()
    Follower_count <- away_followers[[i]]$getFollowersCount()
    
    if (length(Username) > 0) {
        for (j in 1:length(Username)) {
            away_follower_data <- rbind (away_follower_data ,
                                         data.frame(Username = Username[j], 
                                                    Follower_count = Follower_count))
        }
    }
}

away_follower_data <- away_follower_data %>%
    arrange(desc(Follower_count))

library("scales")

away_top20 <- ggplot(data = head(away_follower_data, n = 20), 
                     aes(x = reorder(Username, Follower_count), y = Follower_count, fill = Username))+
    scale_y_continuous(labels = comma) +
    theme_bw() + ylab("Follower Count") + xlab("Username") + ggtitle("@Away Top 20 Followers") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_bar(stat = "identity") +
    guides(fill = FALSE)