# Assigning batting.csv to batting

library(dplyr)
library(ggplot2)

batting <- read_csv('Batting.csv')

# checking the structure how it looks

View(batting)

str(batting)

# column names has numeric number in the start which will give us some problem so lets get rid of it


colnames(batting) <- make.names(colnames(batting))

# Now we have clean standard column names which will be easier to manipulate
# note we could have used transmute() function instead of colnames()

# We need to add three statistics used in Moneyball

# Betting Average

batting$BA <- batting$H / batting$AB

# On base percentage 

batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B which we will need for slugging average

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Slugging average

batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

# Now checking the view of the table

View(batting)

# Now we need to combine salary.csv to batting data frame1

sal <- read.csv('Salaries.csv')

new_data <- merge(batting,sal,by=c('playerID','yearID'))

# in our data the salary data starts from 1985 so lets take out the remaining data and filter it out

combo <- subset(new_data,yearID >= 1985)

# We lost 3 key players so we need to see their stats to know what we want in future players

lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))

# to make the job simpler lets only go for 2001 because that was the year we lost these players

lost_players = subset(lost_players,yearID == 2001)

# Replacement requirements
# >15 million
# AB >= then lost players
# OBP >= mean OBP of lost players

avail.players <- filter(combo,yearID ==2001)

pl <- ggplot(avail.player,aes(x=OBP,y=salary)) + geom_point(color='red',size=1,shape=1)

# it seems like there is no point in giving more then 8 million and we need to get rid of players with OBP=0

avail.players <- filter(avail.players,salary<8000000,OBP>0)

# The total AB of lost players is 1469, meaning i need to cut off avial player by 1500/3 = 500AB

avail.players <- filter(avail.players,AB >= 500)

possible <- head(arrange(avail.players,desc(OBP)),10)

# we make a possible list with 10 players with their OBP in highest to lowest order
# I think i have made my choice as below, theres no wrong answer but thats my best choice

final_answer <- possible[2:4,]

pl2 <- ggplot(final_answer,aes(y=AB, x=salary)) + geom_col(aes(color=playerID,fill=playerID),alpha=0.5)

print(pl2)










