#install package dependencies 
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
#import batting data 
batting <- read.csv('Batting.csv')

#view head of data
head(batting)

#view structure of data
str(batting)

#view head of at bats and X2B columns 
head(batting$AB)

head(batting$X2B)


#calculate batting average
batting$BA <- batting$H / batting$AB

#show tail of data set
tail(batting$BA,5)

#create on base percentage
batting$OBP<- batting$H+batting$BB+batting$HBP/batting$AB+batting$BB+batting$HBP+batting$SF

#show last 5 rows of data 
tail(batting$OBP,5)

#create X1B singles
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

#create slugging average 
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

#show data structure 
str(batting)

#load salary data 
salaries<- read.csv("Salaries.csv")

#show head of data set 
head(salaries)

#view summarized data  
summary(batting)

#clean data to only include data from 1985 and on

batting2<- subset(batting,yearID >= 1985)

#show data summary with min year at 1985
summary(batting2)

#merge salary and batting data frames
combo<- merge(batting2,salaries,by=c("playerID","yearID"))

#view new combined summary data
summary(combo)

#grab the lost players from the new merged data 
lost_players<- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))

print(lost_players)

#only grab lost players data from 2001
lost_players <- subset(lost_players,yearID == 2001)

#check data 
head(lost_players)

#trim down data 
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]

#check data
head(lost_players)

#filter available players
avail.players <- filter(combo,yearID==2001)

#plot available players by obp and salary 
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

#filter by salary above 8 million  and 0 obp
avail.players <- filter(avail.players,salary<8000000,OBP>0)

#filter further by average AB of lost players - 1500/3= 500 AB. 
avail.players <- filter(avail.players,AB >= 500)

#sort possible matches 
possible <- head(arrange(avail.players,desc(OBP)),10)

#grab columns 


possible <- possible[,c('playerID','OBP','AB','salary')]

#show data
print(possible)

#can't choose a lost player again so grab 2-4, show results
possible[2:4,]



