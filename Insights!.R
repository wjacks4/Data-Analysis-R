library(xlsx)
library(tidyverse)
library(plyr)
library(ggplot2)
library(reshape)
library(xts)
library(stargazer)
library(pander)
library(plotrix)
library(caret)
library(caTools)
library(scales)

##IMPORT DATA SETS##
fulldatav1<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/fullcombined.csv", stringsAsFactors = FALSE)
fulldatav2<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/fullcombinedPLUS.csv", stringsAsFactors=FALSE)
contractsyears<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Contract Signings Combined.csv", stringsAsFactors = FALSE)

franchisedata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/BASIC DATA/Franchise Tag Costs.csv")

fulldatav2$Position<-ifelse(fulldatav2$Player=="Randy Starks", "DT", fulldatav2$Position)
fulldatav2$FranchisePosition<-ifelse(fulldatav2$Player=="Randy Starks", "DT", fulldatav2$FranchisePosition)

exptest<-subset(fulldatav2, select=c(Player, Team, Year, FranchisePosition, Experience))

#ADD AGG SNAP DATA#
fulldatav2<-fulldatav2[order( fulldatav2$Team, fulldatav2$Year, fulldatav2$Player,fulldatav2$Week),]
fulldatav2$Total.Snaps<-as.numeric(fulldatav2$Total.Snaps)
snapsub<-subset(fulldatav2, select=c(Player, Team, Year, Total.Snaps, Week))
snapsub<-subset(snapsub, !is.na(snapsub$Total.Snaps))
snapsub$seasonsnaps<-NA

seasonsnaps<-0
for (i in (1:nrow(snapsub))){
  player<-snapsub$Player[i]
  week<-snapsub$Week[i]
  snaps<-snapsub$Total.Snaps[i]
  if (player==snapsub$Player[i+1] & week!=snapsub$Week[i+1] & i<nrow(snapsub)){
    seasonsnaps<-snaps+seasonsnaps
  } else if (player!=snapsub$Player[i+1] & week==snapsub$Week[i+1] & i<nrow(snapsub)){
    seasonsnaps<-snaps+seasonsnaps
    snapsub$seasonsnaps[i]<-seasonsnaps
    seasonsnaps<-0
  } else if (player!=snapsub$Player[i+1] & week!=snapsub$Week[i+1] & i<nrow(snapsub)){
    seasonsnaps<-snaps+seasonsnaps
    snapsub$seasonsnaps[i]<-seasonsnaps
    seasonsnaps<-0
  } else if (i==nrow(snapsub)){
    seasonsnaps<-snaps+seasonsnaps
    snapsub$seasonsnaps[i]<-seasonsnaps
    seasonsnaps<-0
  } 
}

snapsub<-subset(snapsub, !is.na(snapsub$seasonsnaps))
snapsub<-subset(snapsub, select=-c(Total.Snaps, Week))
fulldatav2<-merge(fulldatav2, snapsub, by=c("Player", "Team", "Year"), all.x=TRUE)

#CLEAN DATASETS#
franchisedata$Franchise.Tag.Price<-gsub("\\$", "", franchisedata$Franchise.Tag.Price)
franchisedata$Franchise.Tag.Price<-gsub(",", "", franchisedata$Franchise.Tag.Price)

franchisedata$FranchiseTagPrice<-as.numeric(franchisedata$Franchise.Tag.Price)
franchisedata<-subset(franchisedata, select=-c(Franchise.Tag.Price))


#CREATE WEEK-STRIPPED DATASETS#
fulldatav1NW<-subset(fulldatav1, select=-c(Week, Points, Total.Snaps, Injury.listing, Started))
fulldatav2NW<-subset(fulldatav2, select=-c(Week, Points, Total.Snaps, Injury.listing, Started))

fulldatav1NW<-unique(fulldatav1NW)
fulldatav2NW<-unique(fulldatav2NW)

fulldatav1NW$FranchiseTag<-as.factor(fulldatav1NW$FranchiseTag)
fulldatav1NW$NEFranchiseTagDeclined<-as.factor(fulldatav1NW$NEFranchiseTagDeclined)
fulldatav1NW$NEFranchiseTag<-as.factor(fulldatav1NW$NEFranchiseTag)
fulldatav1NW$FranchiseTagRescinded<-as.factor(fulldatav1NW$FranchiseTagRescinded)
fulldatav1NW$AnyTag<-as.factor(fulldatav1NW$AnyTag)

fulldatav2NW$FranchiseTag<-as.factor(fulldatav2NW$FranchiseTag)
fulldatav2NW$NEFranchiseTagDeclined<-as.factor(fulldatav2NW$NEFranchiseTagDeclined)
fulldatav2NW$NEFranchiseTag<-as.factor(fulldatav2NW$NEFranchiseTag)
fulldatav2NW$FranchiseTagRescinded<-as.factor(fulldatav2NW$FranchiseTagRescinded)
fulldatav2NW$AnyTag<-as.factor(fulldatav2NW$AnyTag)

fulldatav1tags<-subset(fulldatav1NW, fulldatav1NW$AnyTag==1)
fulldatav2tags<-subset(fulldatav2NW, fulldatav2NW$AnyTag==1)


#CREATE AGGREGATE FANTASY STATS#

#####################VISUALS FOR POWERPOINT / DEFENSE###################################

#LEAGUE EARNINGS#
leaguerev<-data.frame(as.factor((c("NFL", "NBA", "MLB", "NHL", "MLS"))))
colnames(leaguerev)[1]<-"League"

#FRANCHISE TAGS BY POSITION#
Tags<-subset(fulldatav2tags, select=c(Player, Team, Year, FranchisePosition, Tag))
Tags<-unique(Tags)
Tags$TagFactor<-as.factor(Tags$Tag)
Tags$Tagtypesimple<-ifelse(Tags$TagFactor=="NEFranchiseTagDeclined", "Non-exclusive Franchise Tag", as.character(Tags$TagFactor))
Tags$Tagtypesimple<-ifelse(Tags$TagFactor=="NEFranchiseTag", "Non-exclusive Franchise Tag", as.character(Tags$Tagtypesimple))
Tags$Tagtypesimple<-ifelse(Tags$TagFactor=="FranchiseTag", "Franchise Tag", as.character(Tags$Tagtypesimple))
Tags$Tagtypesimple<-ifelse(Tags$TagFactor=="FranchiseTagRescinded", "Franchise Tag", as.character(Tags$Tagtypesimple))
Tags$Tagtypesimple<-ifelse(Tags$TagFactor=="TransitionTag", "Transition Tag", as.character(Tags$Tagtypesimple))

Tagnumbers<-data.frame(Tag_Type= c("Exclusive Franchise Tag", "Non-exclusive Franchise Tag", "Transition Tag"), 
                       Frequency=c(0,0,0))

Franchise<-data.frame(which(Tags$Tagtypesimple=="Franchise Tag"))
Franchisefreq<-nrow(Franchise)

NEFranchise<-data.frame(which(Tags$Tagtypesimple=="Non-exclusive Franchise Tag"))
NEFranchisefreq<-nrow(NEFranchise)

Transition<-data.frame(which(Tags$Tagtypesimple=="Transition Tag"))
Transitionfreq<-nrow(Transition)

Tagnumbers$Frequency<-ifelse(Tagnumbers$Tag_Type=="Exclusive Franchise Tag", Franchisefreq, Tagnumbers$Frequency)
Tagnumbers$Frequency<-ifelse(Tagnumbers$Tag_Type=="Non-exclusive Franchise Tag", NEFranchisefreq, Tagnumbers$Frequency)
Tagnumbers$Frequency<-ifelse(Tagnumbers$Tag_Type=="Transition Tag", Transitionfreq, Tagnumbers$Frequency)

ggplot(Tagnumbers, aes(x="", y=Frequency, fill=Tag_Type)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  scale_fill_discrete(name="Tag Type")

Positionfreq<-data.frame(Position=unique(Tags$FranchisePosition), Frequency=rep(0, 11)) 

for (i in 1:nrow(Positionfreq)){
  print(i)
  pos<-Positionfreq$Position[i]
  numobs<-nrow(data.frame(which(Tags$FranchisePosition==pos)))
  Positionfreq$Frequency[i]<-numobs
}

Positionfreq$Position<-factor(Positionfreq$Position, levels=Positionfreq$Position[order(Positionfreq$Frequency)])

ggplot(Positionfreq, aes(x=Position, y=Frequency)) +
  theme_bw() +
  geom_bar(stat="identity") +
  scale_y_continuous() +
  scale_fill_discrete(name="Tag Type") +
  labs(x="Position",
       title="Tag usage by Position (2011-2016)")

#Franchise Tag Prices By Year#
Years<-as.factor(franchisedata$Year)

franchisedata$FranchisePrice2<-(franchisedata$FranchiseTagPrice/1000000)

FranchisePrices<-ggplot(franchisedata, aes(x=Years, y=FranchisePrice2, colour=FranchisePosition, group=FranchisePosition)) +
  theme_grey() +
  geom_line() + 
  geom_point() + 
  labs(y="Franchise Tag Price (in millions of dollars)") +
  labs(colour="Position")

FranchisePrices


#Franchise Tags By Year...Different Formatting#
SampleYears<-as.factor(fulldatav2tags$Year)

SampleYears<-data.frame(unique(fulldatav2tags$Year))
SampleYears$TagTypes<-""
colnames(SampleYears)[1]<-"Year"
test<-SampleYears[order("Year"),]

for (i in 1:nrow(SampleYears)){
  year<-SampleYears$Year[i]
  tags<-length(which(fulldatav2tags$Year==year))
  Ftags<-length(which(fulldatav2tags$Year==year & (fulldatav2tags$FranchiseTag==1 | fulldatav2tags$FranchiseTagRescinded==1)))
  NEFtags<-length(which(fulldatav2tags$Year==year & (fulldatav2tags$NEFranchiseTag==1 | fulldatav2tags$NEFranchiseTagDeclined==1)))
  Ttags<-length(which(fulldatav2tags$Year==year & fulldatav2tags$TransitionTag==1))
  SampleYears$TotalTags[i]<-tags
  SampleYears$FranchiseTags[i]<-Ftags
  SampleYears$NEFranchiseTags[i]<-NEFtags
  SampleYears$TransitionTags[i]<-Ttags
}

#Franchise Tag Combined Graph#
franchisedata$Position<-as.character(franchisedata$FranchisePosition)
franchisedata$averageprice<-0
franchisedata<-franchisedata[order(franchisedata$Year, franchisedata$FranchisePosition),]

SampleYears$AvgPrice<-NA
aggprice=0
nextyear=2011
outputindex<-0
for (i in 1:nrow(franchisedata)){
  year<-franchisedata$Year[i]
  position<-franchisedata$Position[i]
  price<-franchisedata$FranchisePrice2[i]
  nextyear<-franchisedata$Year[i+1]
  tags<-length(which(fulldatav2tags$Year==year & fulldatav2tags$FranchisePosition==position))
  franchisedata$Tags[i]<-tags
  if (franchisedata$Year[i]==nextyear && i<=(nrow(franchisedata)-1)){
    aggprice<-aggprice+price
    #print(aggprice)
    franchisedata$averageprice[i]<-(aggprice/11)
  } else if (franchisedata$Year[i]!=franchisedata$Year[i+1] && i<=(nrow(franchisedata))-1){
    aggprice<-aggprice+price
    franchisedata$averageprice[i]<-(aggprice/11)
    outputindex<-which(SampleYears$Year==year)
    SampleYears$AvgPrice[outputindex]<-(aggprice/11)
    outputindex<-(outputindex+1)
    aggprice<-0
  } else {
    aggprice<-aggprice+price
    franchisedata$averageprice[i]<-(aggprice/11)
    outputindex<-which(SampleYears$Year==year)
    SampleYears$AvgPrice[outputindex]<-(aggprice/11)
  }
}

SampleYears$AvgPrice<-round(SampleYears$AvgPrice, 2)

franchisedata<-merge(SampleYears, franchisedata, by=c("Year"))

FranchiseCombined<-ggplot(SampleYears, aes(x=Year, y=AvgPrice, colour="Average Franchise Tag Price")) +
  geom_line(aes(y=AvgPrice)) +
  geom_point(aes(y=AvgPrice)) +
  labs(y="Average Franchise Tag Price (in millions of dollars)") +
  geom_line(aes(y=TotalTags, colour="Total Tags"), size=1) +
  geom_point(aes(y=TotalTags)) +
  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name="Number of Tags")) 


FranchiseCombined


##############################################################MODEL EVALUATION#####################################################
fulldatav2$NonFranchiseCaphits<-(fulldatav2$samplecaps-fulldatav2$Franchise.Tag.Price)
fulldatav2$NonPlayerOutput<-(fulldatav2$TotalScore-(fulldatav2$seasontotal))
fulldatav2$PlayerTotalOutput<-(fulldatav2$seasontotal)

Modeldataset<-subset(fulldatav2, select=c(NonFranchiseCaphits, Cap, Adjusted.cap, W, Line, TotalScore, seasontotal, Player, Team, 
                                              Year, Franchise.Tag.Price, NonPlayerOutput, Tag, PlayerTotalOutput, 
                                          FranchiseTag, Cap.Hit, Freeagent, Position, AnyTag, FranchisePosition, seasonsnaps, Experience,
                                          IR, samplecaps, deadsamplecaps, Guaranteed, Contract.Face.Value, Length.of.contract..years., draft_year,
                                          draft_round, Probowl))

####CREATE NEW VARIABLES##############
Modeldataset<-unique(Modeldataset)
Modeldataset$Cap.Hit<-as.numeric(Modeldataset$Cap.Hit)
Modeldataset<-subset(Modeldataset, !is.na(Modeldataset$Cap.Hit))
Modeldataset<-Modeldataset[order(Modeldataset$Player, Modeldataset$Year, Modeldataset$Team),]

fulldatav2$Player<-ifelse(fulldatav2$Player=="Brandon Marshall" & fulldatav2$Team=="DEN" & fulldatav2$Year>=2012, "Brandon Marshall LB", fulldatav2$Player)

seasonscores<-subset(Modeldataset, !is.na(Modeldataset$seasontotal))

for(i in 1:nrow(seasonscores)){
  score<-seasonscores$seasontotal[i]
  player<-seasonscores$Player[i]
  if (player==seasonscores$Player[i+1] & i<nrow(seasonscores)){
    seasonscores$lastyearscoring[i+1]<-score
  } else if (player!=seasonscores$Player[i+1] & i<nrow(seasonscores)){
    seasonscores$lastyearscoring[i+1]<-NA
  }
}

Modeldataset<-seasonscores

Modeldataset<-Modeldataset[order(Modeldataset$Year, Modeldataset$FranchisePosition),]
Modeldataset$positionavgscore<-NA
Modeldataset$positionavgcap<-NA
aggscore<-0
aggcap<-0
posindex<-0
for(i in 1:nrow(Modeldataset)){
  score<-Modeldataset$seasontotal[i]
  cap<-Modeldataset$Cap.Hit[i]
  pos<-Modeldataset$FranchisePosition[i]
  if (pos==Modeldataset$FranchisePosition[i+1] & i<nrow(Modeldataset)){
    aggscore<-score+aggscore
    aggcap<-cap+aggcap
    posindex<-posindex+1
  } else if (pos!=Modeldataset$FranchisePosition[i+1] & i<nrow(Modeldataset)){
    aggscore<-score+aggscore
    aggcap<-cap+aggcap
    Modeldataset$positionavgscore[i]<-(aggscore/posindex)
    Modeldataset$positionavgcap[i]<-(aggcap/posindex)
    posindex<-0
    aggscore<-0
    aggcap<-0
  } else {
    aggscore<-score+aggscore
    aggcap<-cap+aggcap
    Modeldataset$positionavgscore[i]<-(aggscore/posindex)
    Modeldataset$positionavgcap[i]<-(aggcap/posindex)
  }
}

Standarddata<-Modeldataset

posavgs<-subset(Modeldataset, !is.na(Modeldataset$positionavgscore))
Modeldataset<-subset(Modeldataset, select=-c(positionavgscore, positionavgcap))
posavgs<-subset(posavgs, select=c(Year, FranchisePosition, positionavgscore, positionavgcap))

Modeldataset<-merge(Modeldataset, posavgs, by=c("Year", "FranchisePosition"), all.x=TRUE)

Modeldataset$scoreoveravg<-Modeldataset$seasontotal-Modeldataset$positionavgscore
Modeldataset$capoveravg<-Modeldataset$Cap.Hit-Modeldataset$positionavgcap

Modeldataset$scoreoveravgpct<-Modeldataset$seasontotal/Modeldataset$positionavgscore
Modeldataset$capoveravgpct<-Modeldataset$Cap.Hit/Modeldataset$positionavgcap

######################USE MODELDATASET FROM HERE ON OUT################################

#BUILD FRANCHISE PLAYERS FULL HISTORY#
Tagplayers<-Modeldataset[which(Modeldataset$AnyTag==1),]


Franchiseplayers<-Modeldataset[which(Modeldataset$FranchiseTag==1),]
Franchiseplayers<-subset(Franchiseplayers, select=c(Player))

Franchiseplayers<-merge(Franchiseplayers, Modeldataset, by="Player")
Franchiseplayers<-subset(Franchiseplayers, select=c(Player, Team, Year, AnyTag, Freeagent, Tag))
Franchiseplayers<-Franchiseplayers[order(Franchiseplayers$Player, Franchiseplayers$Year),]


#####FRANCHISE PLAYERS COMPARED TO REGULAR PLAYERS########
testsub<-subset(Modeldataset, Modeldataset$AnyTag==1)

testsub$Cap.Hitinmil<-(testsub$Cap.Hit/1000000)
testsub$positionavgcapinmil<-(testsub$positionavgcap/1000000)
testsub$capoveravginmil<-(testsub$capoveravg/1000000)

testsub$scoredif<-testsub$seasontotal-testsub$positionavgscore
testsub$scoredifpct<-testsub$scoredif/100
testsub$FranchisePositionFactor<-as.factor(testsub$FranchisePosition)

testsub$ID<-paste(testsub$Player, testsub$Year, sep=" ")
testsub$ID<-as.factor(testsub$ID)

  test2<-melt(testsub, id="ID")
  test2<-subset(test2, test2$variable=="seasontotal" | test2$variable=="positionavgscore")
  test2$value<-as.numeric(test2$value)

  ggplot(test2, aes(x=ID, y=value, group=variable, fill=variable))+
    geom_bar(stat="identity", position="dodge") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) + 
    labs(y="fantasy scores", x="player & year")
  
  
  testdif<-subset(testsub, select=c(Player, Year, FranchisePositionFactor, IR, seasontotal, positionavgscore, Cap.Hit, positionavgcap, scoredifpct))
  testdif<-subset(testdif, testdif$Year>2011)
  ggplot(data=testdif, aes(x=FranchisePositionFactor, y=scoredifpct, fill=FranchisePositionFactor)) +
    geom_boxplot() +
    scale_fill_discrete(name="Position") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(y="Franchise player score percent above / below position average (2012-2016)")
    
  
  capsub<-melt(testsub, id="ID")
  capsub<-subset(capsub, capsub$variable=="Cap.Hit" | capsub$variable=="positionavgcap")
  capsub$value<-as.numeric(capsub$value)
  
  ggplot(capsub, aes(x=ID, y=value, group=variable, fill=variable))+
    geom_bar(stat="identity", position='dodge')+
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    labs(y="salary cap hits (in millions", x="player & year")

  overavg<-melt(testsub, id="ID")
  overavg<-(subset(overavg, overavg$variable=="scoreoveravgpct" | overavg$variable=="capoveravgpct"))
  tagtypes<-subset(testsub, select=c(ID, Tag))
  tagtypes$Tag<-as.factor(tagtypes$Tag)
  overavg<-merge(overavg, tagtypes, by=c("ID"), all.x=TRUE)
  
  overavg$value<-as.numeric(overavg$value)
  
  ggplot(overavg, aes(x=ID, y=value, fill=variable)) +
    geom_bar(stat="identity", position="dodge") +
    labs(y="", x= "player & year") +
    facet_wrap( ~ Tag, strip.position = "bottom", scales="free_x") +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          panel.spacing=unit(0, "lines"),
          strip.background=element_blank(),
          strip.placement = "outside")
  

###################DISTRIBUTION OF CAP & SCORE BY POSITION#######################
Capdensity<-subset(Modeldataset, Modeldataset$Year>2011)
Capdensity<-subset(Capdensity, select=c(Player, Team, Year, FranchisePosition, Cap.Hit))
Capdensity$FranchisePositionFactor<-as.factor(Capdensity$FranchisePosition)
Capdensity<-subset(Capdensity, !is.na(Capdensity$Cap.Hit))

ggplot(Capdensity, aes(x=Cap.Hit)) +
  geom_density() 
  

Scoredensity<-subset(Modeldataset, Modeldataset$Year>2011)
Scoredensity<-subset(Scoredensity, select=c(Player, Team, Year, FranchisePosition, seasontotal))
Scoredensity$FranchisePositionFactor<-as.factor(Scoredensity$FranchisePosition)
Scoredensity<-subset(Scoredensity, !is.na(Scoredensity$seasontotal))

ggplot(Scoredensity, aes(x=seasontotal)) +
  geom_density()

ggplot(Scoredensity, aes(x=seasontotal, fill=FranchisePositionFactor)) +
  geom_density(alpha=.25) +
  scale_fill_discrete(name= "Position") +
  labs(x="Season total Fantasy Score")
    
####COMPARE MAXSCORE PREDICTED FT PLAYERS TO ACTUAL FRANCHISE PLAYERS BY LASTYEARSCORING###
find<-Modeldataset[which(Modeldataset$Player=="Josh Scobee"),]

FAdata<-Modeldataset

FAdata$lastyearscoring<-ifelse(is.na(FAdata$lastyearscoring), 0, FAdata$lastyearscoring)

FAdata$NonPlayerSpending<-(FAdata$samplecaps-FAdata$Cap.Hit)
FAdata$NonPlayerOutput<-(FAdata$TotalScore-FAdata$seasontotal)

FAdata<-FAdata[order(FAdata$Player, FAdata$Year),]
FAdata$Freeagent<-ifelse(is.na(FAdata$Freeagent), "-", FAdata$Freeagent)
FAdata<-subset(FAdata, select=c(Player, Team, Year, Freeagent, Franchise.Tag.Price, AnyTag, ftvalue,# capspace,
                                lastyearscoring, Adjusted.cap, NonPlayerSpending, seasonsnaps, Position, Tag, FranchisePosition)) 
                        
FAdata$FAprior<-"-"
for (i in 1:nrow(FAdata)){
  player<-FAdata$Player[i]
  FAstatus<-FAdata$Freeagent[i]
  if(player==FAdata$Player[i+1] & i<nrow(FAdata) & FAstatus=="1"){
    FAdata$FAprior[i+1]<-"1"
  }
}

FAdata$FAprior<-ifelse(FAdata$AnyTag==1, 1, FAdata$FAprior)

FAdata<-subset(FAdata, FAdata$FAprior=="1")

FAdata$ftvalue<-(FAdata$lastyearscoring/FAdata$Franchise.Tag.Price)*100000
FAdata$ftvalue<-ifelse(is.na(FAdata$ftvalue), 0, FAdata$ftvalue)
FAdata<-FAdata[order(FAdata$Team, FAdata$Year),]

ftvaluemax<-0
FAdata$ftvaluemax<-0
FAdata$ftplayermax<-NA
FAdata$ftplayermaxpos<-NA
FAdata$ftlastyearmax<-0
for (i in 1:nrow(FAdata)){
  ftvalue<-FAdata$ftvalue[i]
  ftplayer<-FAdata$Player[i]
  ftplayerpos<-FAdata$Position[i]
  ftlastyear<-FAdata$lastyearscoring[i]
  year<-FAdata$Year[i]
  if (year==FAdata$Year[i+1] & i<nrow(FAdata)){
    if (ftvalue>=ftvaluemax) {
      ftvaluemax<-ftvalue
      ftplayermax<-ftplayer
      ftplayermaxpos<-ftplayerpos
      ftlastyearmax<-ftlastyear
    }
  } else if (year!= FAdata$Year[i+1] & i<nrow(FAdata)){
    if (ftvalue>=ftvaluemax){
      ftvaluemax<-ftvalue
      ftplayermax<-ftplayer
      ftplayermaxpos<-ftplayerpos
      ftlastyearmax<-ftlastyear
      
      FAdata$ftvaluemax[i]<-ftvaluemax
      FAdata$ftplayermax[i]<-ftplayermax
      FAdata$ftplayermaxpos[i]<-ftplayermaxpos
      FAdata$ftlastyearmax[i]<-ftlastyearmax
    }
    
    FAdata$ftplayermax[i]<-ftplayermax
    FAdata$ftvaluemax[i]<-ftvaluemax
    FAdata$ftplayermaxpos[i]<-ftplayermaxpos
    FAdata$ftlastyearmax[i]<-ftlastyearmax
    
    ftlastyearmax<-0
    ftvaluemax<-0
  } else if (i==nrow(FAdata)){
    if (ftvalue>=ftvaluemax){
      ftvaluemax<-ftvalue
      ftplayermax<-ftplayer
      ftplayermaxpos<ftplayerpos
      FAdata$ftvaluemax[i]<-ftvaluemax
      FAdata$ftplayermax[i]<-ftplayermax
      FAdata$ftplayermaxpos[i]<-ftplayermaxpos
      FAdata$ftlastyearmax[i]<-ftlastyearmax
    }
    FAdata$ftplayermax[i]<-ftplayermax
    FAdata$ftvaluemax[i]<-ftvaluemax
    FAdata$ftplayermaxpos[i]<-ftplayermaxpos
    FAdata$ftlastyearmax[i]<-ftlastyearmax
  }
}

tagstest<-FAdata[which(FAdata$AnyTag==1),]

comparison<-subset(FAdata, !is.na(FAdata$ftplayermax))
comparison<-subset(comparison, select=c(ftplayermax, Team, Year, ftvaluemax, ftlastyearmax, ftplayermaxpos))
tagged<-subset(FAdata, FAdata$AnyTag==1)
tagged<-subset(tagged, select=c(Player, Team, Year, ftvalue, lastyearscoring, Tag, Position))
colnames(tagged)[5]<-"Franchiselastyearscoring"
colnames(tagged)[4]<-"Franchiseplayervalue"
colnames(tagged)[7]<-"TaggedPosition"

comparisonmerge<-merge(tagged, comparison, by=c("Team", "Year"), all.x=TRUE)

comparisonmerge$correct<-0

for (i in 1:nrow(comparisonmerge)){
  Taggedplayer<-comparisonmerge$Player[i]
  Predictedplayer<-comparisonmerge$ftplayermax[i]
  if (Taggedplayer==Predictedplayer){
    comparisonmerge$correct[i]<-1
  } else {
    comparisonmerge$correct[i]<-0
  }
}

comparisonmergeNO2011<-subset(comparisonmerge, comparisonmerge$Year!=2011)

comparisonmergeNO2011$ID<-NA
comparisonmergeNO2011$ID<-paste(comparisonmergeNO2011$Team, comparisonmergeNO2011$Year)

comparisonmergeNO2011$correct1<-as.factor(comparisonmergeNO2011$correct)
ggplot(comparisonmergeNO2011, aes(x=correct1))+
  geom_histogram(stat="count")


wrong<-subset(comparisonmergeNO2011, comparisonmergeNO2011$correct==0)
wrong$ftvaluemax<-as.factor(wrong$ftvaluemax)
wrong$Franchiseplayervalue<-as.factor(wrong$Franchiseplayervalue)
wrong<-subset(wrong, select=c(ID, Franchiseplayervalue, ftvaluemax, ftplayermaxpos, TaggedPosition, Player, ftplayermax))
wrongmelt<-melt(wrong, id="ID")

wrongplot1<-subset(wrongmelt, wrongmelt$variable=="Franchiseplayervalue" | wrongmelt$variable=="ftvaluemax")
wrongplot1$value<-as.numeric(as.character(wrongplot1$value))

posmerge1<-subset(wrongmelt, wrongmelt$variable=="TaggedPosition")
colnames(posmerge1)[2]<-"Playertype"
colnames(posmerge1)[3]<-"Position"
posmerge1$variable<-"Franchiseplayervalue"

posmerge2<-subset(wrongmelt, wrongmelt$variable=="ftplayermaxpos")
colnames(posmerge2)[2]<-"Playertype"
colnames(posmerge2)[3]<-"Position"
posmerge2$variable<-"ftvaluemax"

wrongplot1<-merge(wrongplot1, posmerge1, by=c("ID", "variable"), all.x=TRUE)
wrongplot1<-merge(wrongplot1, posmerge2, by=c("ID", "variable"), all.x=TRUE)
wrongplot1$Position1<-as.character(wrongplot1$Position.x)
wrongplot1$Position2<-as.character(wrongplot1$Position.y)

wrongplot1$Position1<-ifelse(is.na(wrongplot1$Position1), wrongplot1$Position2, wrongplot1$Position1)

wrongplot1<-subset(wrongplot1, select=c(ID, variable, value, Position1))

wrongplot1<-ggplot(wrongplot1, aes(x=ID, y=value, group=variable, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Position1,
                hjust=ifelse(variable=="Franchiseplayervalue", 1.25, -.25),
                vjust=2)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

wrongplot1

    ##TRY SAME PROCESS BUT WITHOUT KICKERS##

    FAdataNOK<-Modeldataset
    FAdataNOK$lastyearscoring<-ifelse(is.na(FAdataNOK$lastyearscoring), 0, FAdataNOK$lastyearscoring)

    FAdataNOK<-FAdataNOK[order(FAdataNOK$Player, FAdataNOK$Year),]
    FAdataNOK$Freeagent<-ifelse(is.na(FAdataNOK$Freeagent), "-", FAdataNOK$Freeagent)
    FAdataNOK<-subset(FAdataNOK, select=c(Player, Team, Year, Freeagent, Franchise.Tag.Price, AnyTag, ftvalue,# capspace,
                                    lastyearscoring, Adjusted.cap, seasonsnaps, Position, Tag, FranchisePosition)) 
    
    FAdataNOK$FAprior<-"-"
    for (i in 1:nrow(FAdataNOK)){
      player<-FAdataNOK$Player[i]
      FAstatus<-FAdataNOK$Freeagent[i]
      if(player==FAdataNOK$Player[i+1] & i<nrow(FAdataNOK) & FAstatus=="1"){
        FAdataNOK$FAprior[i+1]<-"1"
      }
    }
    
    FAdataNOK$FAprior<-ifelse(FAdataNOK$AnyTag==1, 1, FAdataNOK$FAprior)
    
    FAdataNOK<-subset(FAdataNOK, FAdataNOK$FAprior=="1")
    
    FAdataNOK$ftvalue<-(FAdataNOK$lastyearscoring/FAdataNOK$Franchise.Tag.Price)*100000
    FAdataNOK$ftvalue<-ifelse(is.na(FAdataNOK$ftvalue), 0, FAdataNOK$ftvalue)
    
    #remove kickers here#
    FAdataNOK<-subset(FAdataNOK, FAdataNOK$Position!="K")
    
    FAdataNOK<-FAdataNOK[order(FAdataNOK$Team, FAdataNOK$Year),]
    
    ftvaluemax<-0
    FAdataNOK$ftvaluemax<-0
    FAdataNOK$ftplayermax<-NA
    FAdataNOK$ftplayermaxpos<-NA
    FAdataNOK$ftlastyearmax<-0
    for (i in 1:nrow(FAdataNOK)){
      ftvalue<-FAdataNOK$ftvalue[i]
      ftplayer<-FAdataNOK$Player[i]
      ftplayerpos<-FAdataNOK$Position[i]
      ftlastyear<-FAdataNOK$lastyearscoring[i]
      year<-FAdataNOK$Year[i]
      if (year==FAdataNOK$Year[i+1] & i<nrow(FAdataNOK)){
        if (ftvalue>=ftvaluemax) {
          ftvaluemax<-ftvalue
          ftplayermax<-ftplayer
          ftplayermaxpos<-ftplayerpos
          ftlastyearmax<-ftlastyear
        }
      } else if (year!= FAdataNOK$Year[i+1] & i<nrow(FAdataNOK)){
        if (ftvalue>=ftvaluemax){
          ftvaluemax<-ftvalue
          ftplayermax<-ftplayer
          ftplayermaxpos<-ftplayerpos
          ftlastyearmax<-ftlastyear
          
          FAdataNOK$ftvaluemax[i]<-ftvaluemax
          FAdataNOK$ftplayermax[i]<-ftplayermax
          FAdataNOK$ftplayermaxpos[i]<-ftplayermaxpos
          FAdataNOK$ftlastyearmax[i]<-ftlastyearmax
        }
        
        FAdataNOK$ftplayermax[i]<-ftplayermax
        FAdataNOK$ftvaluemax[i]<-ftvaluemax
        FAdataNOK$ftplayermaxpos[i]<-ftplayermaxpos
        FAdataNOK$ftlastyearmax[i]<-ftlastyearmax
        
        ftlastyearmax<-0
        ftvaluemax<-0
      } else if (i==nrow(FAdataNOK)){
        if (ftvalue>=ftvaluemax){
          ftvaluemax<-ftvalue
          ftplayermax<-ftplayer
          ftplayermaxpos<ftplayerpos
          FAdataNOK$ftvaluemax[i]<-ftvaluemax
          FAdataNOK$ftplayermax[i]<-ftplayermax
          FAdataNOK$ftplayermaxpos[i]<-ftplayermaxpos
          FAdataNOK$ftlastyearmax[i]<-ftlastyearmax
        }
        FAdataNOK$ftplayermax[i]<-ftplayermax
        FAdataNOK$ftvaluemax[i]<-ftvaluemax
        FAdataNOK$ftplayermaxpos[i]<-ftplayermaxpos
        FAdataNOK$ftlastyearmax[i]<-ftlastyearmax
      }
    }
    
    comparison<-subset(FAdataNOK, !is.na(FAdataNOK$ftplayermax))
    comparison<-subset(comparison, select=c(ftplayermax, Team, Year, ftvaluemax, ftlastyearmax, ftplayermaxpos))
    tagged<-subset(FAdataNOK, FAdataNOK$AnyTag==1)
    tagged<-subset(tagged, select=c(Player, Team, Year, ftvalue, lastyearscoring, Tag, Position))
    colnames(tagged)[5]<-"Franchiselastyearscoring"
    colnames(tagged)[4]<-"Franchiseplayervalue"
    colnames(tagged)[7]<-"TaggedPosition"
    
    comparisonmerge<-merge(tagged, comparison, by=c("Team", "Year"), all.x=TRUE)
    
    comparisonmerge$correct<-0
    
    for (i in 1:nrow(comparisonmerge)){
      Taggedplayer<-comparisonmerge$Player[i]
      Predictedplayer<-comparisonmerge$ftplayermax[i]
      if (Taggedplayer==Predictedplayer){
        comparisonmerge$correct[i]<-1
      } else {
        comparisonmerge$correct[i]<-0
      }
    }
    
    comparisonmergeNO2011<-subset(comparisonmerge, comparisonmerge$Year!=2011)
    
    comparisonmergeNO2011$ID<-NA
    comparisonmergeNO2011$ID<-paste(comparisonmergeNO2011$Team, comparisonmergeNO2011$Year)
    
    wrong<-subset(comparisonmergeNO2011, comparisonmergeNO2011$correct==0)
    wrong$ftvaluemax<-as.factor(wrong$ftvaluemax)
    wrong$Franchiseplayervalue<-as.factor(wrong$Franchiseplayervalue)
    wrong<-subset(wrong, select=c(ID, Franchiseplayervalue, ftvaluemax, ftplayermaxpos, TaggedPosition, Player, ftplayermax))
    wrongmelt<-melt(wrong, id="ID")
    
    wrongplot1<-subset(wrongmelt, wrongmelt$variable=="Franchiseplayervalue" | wrongmelt$variable=="ftvaluemax")
    wrongplot1$value<-as.numeric(as.character(wrongplot1$value))
    
    posmerge1<-subset(wrongmelt, wrongmelt$variable=="TaggedPosition")
    colnames(posmerge1)[2]<-"Playertype"
    colnames(posmerge1)[3]<-"Position"
    posmerge1$variable<-"Franchiseplayervalue"
    
    posmerge2<-subset(wrongmelt, wrongmelt$variable=="ftplayermaxpos")
    colnames(posmerge2)[2]<-"Playertype"
    colnames(posmerge2)[3]<-"Position"
    posmerge2$variable<-"ftvaluemax"
    
    wrongplot1<-merge(wrongplot1, posmerge1, by=c("ID", "variable"), all.x=TRUE)
    wrongplot1<-merge(wrongplot1, posmerge2, by=c("ID", "variable"), all.x=TRUE)
    wrongplot1$Position1<-as.character(wrongplot1$Position.x)
    wrongplot1$Position2<-as.character(wrongplot1$Position.y)
    
    wrongplot1$Position1<-ifelse(is.na(wrongplot1$Position1), wrongplot1$Position2, wrongplot1$Position1)
    
    wrongplot1<-subset(wrongplot1, select=c(ID, variable, value, Position1))
    
    wrongplot1NOKickers<-(ggplot(wrongplot1, aes(x=ID, y=value, group=variable, fill=variable)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(label=Position1,
                    hjust=ifelse(variable=="Franchiseplayervalue", 1.25, -.25),
                    vjust=2)) +
      theme(axis.text.x=element_text(angle=45, hjust=1)))
    
    wrongplot1NOKickers
    

#LOGIT REGRESSION OF VALUE ON TAG / NO TAG#
FAdata$AnyTag1<-as.factor(FAdata$AnyTag)
FAdata$Position1<-as.factor(FAdata$FranchisePosition)
FAdata$YearFactor<-as.factor(FAdata$Year)
FAdataNO2011<-subset(FAdata, FAdata$Year>2012)
testlogit<-glm(AnyTag ~ Franchise.Tag.Price + lastyearscoring + YearFactor, data=FAdataNO2011, family="binomial")

summary(testlogit)
#stargazer(testlogit)

probdata<-subset(FAdataNO2011, select=c(Franchise.Tag.Price, lastyearscoring, YearFactor))
probpred<-data.frame(predict(testlogit, newdata=probdata))
colnames(probpred)[1]<-"logit"
probpred$odds<-exp(probpred$logit)
probpred$prob<-(probpred$odds/(1+probpred$odds))

FAdataprob<-cbind(FAdataNO2011, probpred)

ggplot(FAdataprob, aes(lastyearscoring, prob, colour=YearFactor))+
  #stat_smooth(method="glm", family="binomial", formula=y~x, se=FALSE) +
  geom_point()

#ACCOUNT FOR AVAILABILITY OF SIMILAR PLAYERS#
#similarplayers<-Standarddata
#similarplayers<-unique(similarplayers)

#consideredcontracts<-subset(similarplayers, (similarplayers$draft_year<=2012))

#consideredcontracts<-subset(consideredcontracts, !is.na(consideredcontracts$Length.of.contract..years.))
#consideredcontracts<-subset(consideredcontracts, consideredcontracts$Year>2011)
#consideredcontracts<-consideredcontracts[order(consideredcontracts$Player, consideredcontracts$Year),]

#consideredcontracts$FranchisePositionFactor<-as.factor(consideredcontracts$FranchisePosition)
#consideredcontracts$AnyTag1<-as.factor(consideredcontracts$AnyTag)
#consideredcontracts$YearFactor<-as.factor(consideredcontracts$Year)

  #LINEAR MODELS & GRAPHS#
  predcontract<-lm(Length.of.contract..years.~lastyearscoring+FranchisePositionFactor+YearFactor, data=consideredcontracts)

  summary(predcontract)

  ggplot(consideredcontracts, aes(x=lastyearscoring, y=Length.of.contract..years., colour=AnyTag1))+
    geom_point() +
    geom_smooth(method="lm", se=FALSE)
  
  
  predfacevalue<-lm(Contract.Face.Value~lastyearscoring+FranchisePositionFactor+YearFactor, data=consideredcontracts)
  
  summary(predfacevalue)
  
  ggplot(consideredcontracts, aes(x=lastyearscoring, y=Contract.Face.Value, colour=FranchisePosition))+
    geom_point() +
    geom_smooth(method="lm", se=FALSE)
  
  
  predguaranteed<-lm(Guaranteed~lastyearscoring+FranchisePositionFactor+YearFactor, data=consideredcontracts)
  
  summary(predguaranteed)
  
  ggplot(consideredcontracts, aes(x=lastyearscoring, y=Guaranteed, colour=FranchisePosition))+
    geom_point() +
    geom_smooth(method="lm", se=FALSE)

  

taggedsample<-subset(similarplayers, similarplayers$AnyTag==1)
taggedsample$FranchisePositionFactor<-as.factor(taggedsample$FranchisePosition)
taggedsample$YearFactor<-as.factor(taggedsample$Year)

  preddata<-subset(taggedsample, select=c(lastyearscoring, FranchisePositionFactor, YearFactor))
  predcontractlength<-as.data.frame(predict(predcontract, newdata = preddata))
  colnames(predcontractlength)[1]<-"Predicted_Contract_Length"
  predFV<-as.data.frame(predict(predfacevalue, newdata=preddata))
  colnames(predFV)[1]<-"Predicted_Contract_Face_Value"
  predgteed<-as.data.frame(predict(predguaranteed, newdata=preddata))
  colnames(predgteed)[1]<-"Predicted_Guaranteed_Money"

taggedsample<-cbind(taggedsample, predcontractlength, predFV, predgteed)

tagpassmelt<-subset(taggedsample, taggedsample$Tag=="FranchiseTagRescinded" | taggedsample$Tag=="NEFranchiseTagDeclined")
tagpassmelt<-subset(taggedsample, select=c(Predicted_Guaranteed_Money, Player, Year, Franchise.Tag.Price, Tag))


taggedmelt<-subset(taggedsample, select=c(Predicted_Guaranteed_Money, Player, Year, Franchise.Tag.Price, Tag))
taggedmelt<-subset(taggedmelt, !is.na(taggedmelt$Predicted_Guaranteed_Money))
taggedmelt$ID<-paste(taggedmelt$Player, taggedmelt$Year)
taggedmelt<-subset(taggedmelt, select=-c(Player, Year))

#taggedmelt$differential<-taggedmelt$Predicted_Guaranteed_Money-taggedmelt$Franchise.Tag.Price

#taggedmelt<-subset(taggedmelt, select=c(differential, ID))

#t.test(taggedmelt$differential, mu=0, conf.level=.95)

taggedmelt<-melt(taggedmelt, id="ID")
taggedmelt$value<-as.numeric(taggedmelt$value)

ggplot(taggedmelt, aes(x=ID, y=value, group=variable, fill=variable)) +
  geom_histogram(stat="identity", position="dodge") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#EXPAND TO FULL MODEL#
FAdataNO2011<-subset(FAdata, FAdata$Year>2012)

FAdataNO2011<-FAdataNO2011[order(FAdataNO2011$FranchisePosition, FAdataNO2011$YearFactor, FAdataNO2011$lastyearscoring),]

FAdataNO2011$FranchisePositionFactor<-as.factor(FAdataNO2011$FranchisePosition)
  
  predfulldata<-subset(FAdataNO2011, select=c(lastyearscoring, FranchisePositionFactor, YearFactor))
  predfullcontractlength<-as.data.frame(predict(predcontract, newdata=predfulldata))
  colnames(predfullcontractlength)[1]<-"Predicted_Contract_Length"
  predfullcontractFV<-as.data.frame(predict(predfacevalue, newdata=predfulldata))
  colnames(predfullcontractFV)[1]<-"Predicted_Contract_Face_Value"
  predfullguaranteed<-as.data.frame(predict(predguaranteed, newdata=predfulldata))
  colnames(predfullguaranteed)[1]<-"Predicted_Guaranteed_Money"

FAdataNO2011<-cbind(FAdataNO2011, predfullcontractFV, predfullcontractlength, predfullguaranteed)


#PRICE PER FANTASY POINT FUNCTION#
theorydata2<-Modeldataset

theorydata2$FranchiseIndicator<-as.numeric(as.character(theorydata2$FranchiseTag))

theorydata2$NonPlayerSpending<-(theorydata2$samplecaps-theorydata2$Cap.Hit)
theorydata2$NonPlayerOutputt<-(theorydata2$TotalScore-theorydata2$seasontotal)


theorydata2<-subset(theorydata2, !is.na(seasontotal))
theorydata2$outputprice<-(theorydata2$Cap.Hit/theorydata2$seasontotal)

theorydata2<-subset(theorydata2, !is.infinite(theorydata2$outputprice))

  #DISTRIBUTION OF OUTPUT PRICES#
  qnt<-quantile(theorydata2$outputprice, probs=c(.25, .75), na.rm=TRUE)
  UL<-1.5*(IQR(theorydata2$outputprice, na.rm=TRUE))

  qnt[1]-UL

  theorydata2sub<-subset(theorydata2, ((theorydata2$outputprice>=(qnt[1]-UL)) & (theorydata2$outputprice<=(qnt[2]+UL))))

  ggplot(theorydata2sub, aes(x=outputprice)) +
    geom_density()

PriceFunction<-function(cap, nonplayerspending, wins, beta, nonplayeroutput){
  (cap-nonplayerspending)/(((wins)/(beta))-nonplayeroutput)
}

theorydata2$modelprice<-PriceFunction(theorydata2$Cap, theorydata2$NonPlayerSpending, theorydata2$W, .00478, theorydata2$NonPlayerOutput)

##################################################END MODEL EVALUATION#####################################












###MODEL EVALUATION 2.0##
##STEP 1 - EVALUATE EXPLANATORY POWER OF FANTASY SCORING ACROSS POSITIONS##
explaintest<-subset(Modeldataset, select=c(Player, Team, Year, W, FranchisePosition, seasontotal, Line))
explaintest<-subset(explaintest, explaintest$Year>2011)
explaintest<-subset(explaintest, !is.na(explaintest$seasontotal))
explaintest$YearFactor<-as.factor(explaintest$Year)
explaintest$FranchisePositionFactor<-as.factor(explaintest$FranchisePosition)
explaintest<-explaintest[order(explaintest$Team, explaintest$Year),]
explaintest$yeartotal<-NA

explainfull<-explaintest
scoreagg<-0
for (i in 1:nrow(explainfull)){
  score<-explainfull$seasontotal[i]
  year<-explainfull$Year[i]
  if (year==explainfull$Year[i+1] & i<nrow(explainfull)){
    scoreagg<-scoreagg+score
  } else if(year!=explainfull$Year[i+1] & i<nrow(explainfull)){
    scoreagg<-scoreagg+score
    explainfull$yeartotal[i]<-scoreagg
    scoreagg<-0
  } else if(i==nrow(explainfull)){
    scoreagg<-scoreagg+score
    explainfull$yeartotal[i]<-scoreagg
  }
}
allpostotals<-subset(explainfull, select=c(Team, Year, W, yeartotal, Line))
allpostotals<-subset(allpostotals, !is.na(explainfull$yeartotal))
allpostotals<-unique(allpostotals)

explaintest<-explaintest[order(explaintest$Team, explaintest$Year, explaintest$FranchisePosition),]
explainbypos<-explaintest
scoreagg<-0
for (i in 1:nrow(explainbypos)){
  score<-explainbypos$seasontotal[i]
  year<-explainbypos$Year[i]
  pos<-explainbypos$FranchisePosition[i]
  if (pos==explainbypos$FranchisePosition[i+1] & i<nrow(explainbypos)){
    scoreagg<-scoreagg+score
  } else if(pos!=explainbypos$FranchisePosition[i+1] & i<nrow(explainbypos)){
    scoreagg<-scoreagg+score
    explainbypos$yeartotal[i]<-scoreagg
    scoreagg<-0
  } else if(i==nrow(explainbypos)){
    scoreagg<-scoreagg+score
    explainbypos$yeartotal[i]<-scoreagg
  }
}
postotals<-subset(explainbypos, select=c(Team, YearFactor, W, Line, yeartotal, FranchisePositionFactor))
postotals<-subset(postotals, !is.na(explainbypos$yeartotal))
postotals<-unique(postotals)

explaindef<-subset(explaintest, FranchisePosition=="DE" | FranchisePosition=="DT" | FranchisePosition=="CB" | FranchisePosition=="S")
scoreagg<-0
for (i in 1:nrow(explaindef)){
  score<-explaindef$seasontotal[i]
  year<-explaindef$Year[i]
  if (year==explaindef$Year[i+1] & i<nrow(explaindef)){
    scoreagg<-scoreagg+score
  } else if(year!=explaindef$Year[i+1] & i<nrow(explaindef)){
    scoreagg<-scoreagg+score
    explaindef$yeartotal[i]<-scoreagg
    scoreagg<-0
  } else if(i==nrow(explaindef)){
    scoreagg<-scoreagg+score
    explaindef$yeartotal[i]<-scoreagg
  }
}
deftotals<-subset(explaindef, select=c(Team, Year, W, yeartotal, Line))
deftotals<-subset(deftotals, !is.na(explaindef$yeartotal))
deftotals<-unique(deftotals)

explainoff<-subset(explaintest, FranchisePosition=="QB" | FranchisePosition=="WR" | FranchisePosition=="TE" | FranchisePosition=="RB" |
                     FranchisePosition=="OL")
scoreagg<-0
for (i in 1:nrow(explainoff)){
  score<-explainoff$seasontotal[i]
  year<-explainoff$Year[i]
  if (year==explainoff$Year[i+1] & i<nrow(explainoff)){
    scoreagg<-scoreagg+score
  } else if(year!=explainoff$Year[i+1] & i<nrow(explainoff)){
    scoreagg<-scoreagg+score
    explainoff$yeartotal[i]<-scoreagg
    scoreagg<-0
  } else if(i==nrow(explainoff)){
    scoreagg<-scoreagg+score
    explainoff$yeartotal[i]<-scoreagg
  }
}
offtotals<-subset(explainoff, select=c(Team, Year, W, yeartotal, Line))
offtotals<-subset(offtotals, !is.na(explainoff$yeartotal))
offtotals$ID<-"OFF"
offtotals<-unique(offtotals)

explainweird<-subset(explaintest, FranchisePosition=="ST")
scoreagg<-0
for (i in 1:nrow(explainweird)){
  score<-explainweird$seasontotal[i]
  year<-explainweird$Year[i]
  if (year==explainweird$Year[i+1] & i<nrow(explainweird)){
    scoreagg<-scoreagg+score
  } else if(year!=explainweird$Year[i+1] & i<nrow(explainweird)){
    scoreagg<-scoreagg+score
    explainweird$yeartotal[i]<-scoreagg
    scoreagg<-0
  } else if(i==nrow(explainweird)){
    scoreagg<-scoreagg+score
    explainweird$yeartotal[i]<-scoreagg
  }
}
weirdtotals<-subset(explainweird, select=c(Team, Year, W, yeartotal, Line))
weirdtotals<-subset(weirdtotals, !is.na(explainweird$yeartotal))
weirdtotals<-unique(weirdtotals)


comparisontotals<-deftotals
comparisontotals$ID<-as.factor("DEF")
offtotals$ID<-"OFF"
weirdtotals$ID<-"ST"

comparisontotals<-rbind(comparisontotals, offtotals, weirdtotals)

ggplot(comparisontotals, aes(y=W, x=yeartotal))+
  geom_jitter() +
  geom_smooth(method=lm, se=FALSE) +
  scale_x_continuous(limits=c(0, 1500)) +
  facet_grid(.~ID)

offtotalscol<-subset(offtotals, select=c(yeartotal))
colnames(offtotalscol)[1]<-"Offense_Season_Total"
weirdtotalscol<-subset(weirdtotals, select=c(yeartotal))
colnames(weirdtotalscol)[1]<-"ST_Season_Total"

regressiontable<-cbind(deftotals, offtotalscol, weirdtotalscol)
#regressiontable$ID<-ifelse(is.na(regressiontable$ID), "DEF", regressiontable$ID)
colnames(regressiontable)[4]<-"Defense_Season_Total"

comparisonreg<-lm(W~Line+Defense_Season_Total+Offense_Season_Total+ST_Season_Total, data=regressiontable)
summary(comparisonreg)

stargazer(comparisonreg)

allposreg<-lm(W~Line+yeartotal, data=allpostotals)
summary(allposreg)


##STEP 2 - NORMALIZE SCORING##
scoredistribution<-subset(Modeldataset, select=c(Team, Year, TotalScore))
scoredistribution<-unique(scoredistribution)
scoredistribution$Year1<-as.factor(scoredistribution$Year)

scoredistribution2015<-subset(scoredistribution, scoredistribution$Year==2015)

shapiro.test(scoredistribution2015$TotalScore)

ggplot(scoredistribution, aes(sample=TotalScore, color=Year1))+
  stat_qq()

ModeldatasetNO2011<-subset(Modeldataset, Modeldataset$Year>2011)
MeanTotalScore<-mean(ModeldatasetNO2011$TotalScore)
StdDevTotalScore<-sd(ModeldatasetNO2011$TotalScore)
ModeldatasetNO2011$normTotalScore<-NA
ModeldatasetNO2011$normTotalScore<-((ModeldatasetNO2011$TotalScore-MeanTotalScore)/StdDevTotalScore)

MeanPlayerScore<-mean(ModeldatasetNO2011$lastyearscoring, na.rm = TRUE)
StdDevPlayerScore<-sd(ModeldatasetNO2011$lastyearscoring, na.rm = TRUE)
ModeldatasetNO2011$normPlayerScore<-NA
ModeldatasetNO2011$normPlayerScore<-((ModeldatasetNO2011$lastyearscoring-MeanPlayerScore)/StdDevPlayerScore)

##STEP 4 - ESTIMATE dW/dA AND ADDITIONAL WINS FOR EVERY PLAYER##
ModeldatasetNO2011$YearFactor<-as.factor(ModeldatasetNO2011$Year)
ModeldatasetNO2011$FranchisePositionFactor<-as.factor(ModeldatasetNO2011$FranchisePosition)

ModeldatasetNO2011$Marginalwinsproduced<-NA
ModeldatasetNO2011$Marginalwinsproduced<-(ModeldatasetNO2011$lastyearscoring*.0047196)

ModeldatasetNO2011<-ModeldatasetNO2011[order(ModeldatasetNO2011$Team, ModeldatasetNO2011$Year),]

##STEP 6 - MEASURE HOW BADLY THE FREE AGENT WILL BE MISSED (HOW MUCH TALET REMAINS ON THE ROSTER IN THEIR POSITION)##
missudata<-ModeldatasetNO2011
missudata<-missudata[order(missudata$Team, missudata$Year, missudata$FranchisePosition),]
positionagg<-0
missudata$positiontotalscore<-NA
for(i in 1:nrow(missudata)){
  position<-missudata$FranchisePosition[i]
  score<-missudata$seasontotal[i]
  if(position==missudata$FranchisePosition[i+1] & i<nrow(missudata)){
    positionagg<-score+positionagg
  } else if(position!=missudata$FranchisePosition[i+1] & i<nrow(missudata)){
    positionagg<-score+positionagg
    missudata$positiontotalscore[i]<-positionagg
    positionagg<-0
  } else {
    positionagg<-score+positionagg
    missudata$positiontotalscore[i]<-positionagg
  }
}

totals<-subset(missudata, !is.na(missudata$positiontotalscore))
totals<-subset(totals, select=c(Team, Year, FranchisePosition, positiontotalscore))
totals<-unique(totals)
missudata<-subset(missudata, select=-c(positiontotalscore))
missudata<-merge(missudata, totals, by=c("Team", "Year", "FranchisePosition"), all.x = TRUE)
missudata$positioncontribution<-missudata$seasontotal/missudata$positiontotalscore
missudata$positioncontribution<-round(missudata$positioncontribution, 3)

missudata<-missudata[order(missudata$Player, missudata$Year),]
missudata$lastyearcontribution<-NA
for(i in 1:nrow(missudata)){
  contribution<-missudata$positioncontribution[i]
  player<-missudata$Player[i]
  position<-missudata$FranchisePosition[i]
  if (player==missudata$Player[i+1] & i<nrow(missudata)){
    missudata$lastyearcontribution[i+1]<-contribution
  } else if (player!=missudata$Player[i+1] & i<(nrow(Modeldataset)-1)){
    missudata$lastyearcontribution[i+1]<-NA
  } else if (i==nrow(Modeldataset)){
    missudata$lastyearcontribution[i]<-NA
  }
}

ModeldatasetNO2011<-missudata

##STEP 7 - MEASURE STANDARD DEVIATION OF SNAPS##
VAR<-subset(fulldatav2, select=c(Player, Team, FranchisePosition, Year, Week, Points, W, Total.Snaps, Injury.listing, IR, seasonsnaps, Started, Line))
VAR$Total.Snaps<-ifelse(is.na(VAR$Total.Snaps), 0, VAR$Total.Snaps)
VAR$Started<-ifelse(is.na(VAR$Started), "NO", VAR$Started)
VAR$Started<-as.factor(VAR$Started)
VAR$Injury.listing<-ifelse(is.na(VAR$Injury.listing), "None", VAR$Injury.listing)
VAR<-subset(VAR, VAR$Year>=2012)
VAR<-VAR[order(VAR$Team, VAR$FranchisePosition, VAR$Year, VAR$Player,VAR$Week),]
VAR$Healthyavg<-NA
VAR$Healthysnaps<-0
VAR$Injured<-0
VAR$Snapsd<-0
VAR$IR<-0

VAR$Injured<-ifelse(VAR$Injury.listing=="Questionable" | VAR$Injury.listing=="Doubtful" | VAR$Injury.listing=="Out" | 
                      VAR$Injury.listing=="Injured Reserve", 1, VAR$Injured)

seasonhealthysnaps<-0
seasoninjuredsnaps<-0
h<-0
u<-0
for(i in 1:nrow(VAR)){
  week<-VAR$Week[i]
  player<-VAR$Player[i]
  injury<-VAR$Injury.listing[i]
  snaps<-VAR$Total.Snaps[i]
  if(player==VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==0){
    seasonhealthysnaps<-seasonhealthysnaps + snaps
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    h<-h+1
  } else if (player==VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==1){
    u<-u+1
  } else if (player!=VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==0){
    seasonhealthysnaps<-seasonhealthysnaps+snaps
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    h<-h+1
    VAR$Healthyavg[i]<-(seasonhealthysnaps/h)
    VAR$Snapsd[i]<-sd(VAR$Total.Snaps[(i-(h+u)+1):i])
    h<-0
    u<-0
    seasonhealthysnaps<-0
  } else if (player!=VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==1){
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    u<-u+1
    VAR$Healthyavg[i]<-(seasonhealthysnaps/h)
    VAR$Snapsd[i]<-sd(VAR$Total.Snaps[(i-(h+u)+1):i])
    h<-0
    u<-0
    seasonhealthysnaps<-0
  } else if (i==nrow(VAR)){
    VAR$Healthyavg[i]<-(seasonhealthysnaps/h)
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    VAR$Snapsd[i]<-sd(VAR$Total.Snaps[(i-(h+u)+1):i])
  }
}

healthyavg<-subset(VAR, !is.na(VAR$Healthyavg))
healthyavg<-subset(healthyavg, select=c(Player, Team, Year, FranchisePosition, Healthyavg))

snapsd<-subset(VAR, !is.na(VAR$Healthyavg))
snapsd<-subset(snapsd, select=c(Player, Team, Year, FranchisePosition, Snapsd, Healthyavg))

VAR<-subset(VAR, select=-c(Healthyavg))
VAR<-merge(VAR, healthyavg, by=c("Player", "Team", "Year", "FranchisePosition"))

##STEP 8 - ISOLATE FREE AGENTS THAT TEAMS CONSIDER - ELIMINATE 2011 B/C ONLY 21 TEAMS FANTASY TOTALS REPRESENTED##
FAdata<-ModeldatasetNO2011

FAdata$lastyearscoring<-ifelse(is.na(FAdata$lastyearscoring), 0, FAdata$lastyearscoring)

FAdata$Freeagent<-ifelse(is.na(FAdata$Freeagent), "-", FAdata$Freeagent)
FAdata<-subset(FAdata, select=c(Player, Team, Year, YearFactor, Freeagent, Franchise.Tag.Price, AnyTag, ftvalue, W, seasontotal,
                                lastyearscoring, Adjusted.cap, seasonsnaps, Position, Tag, FranchisePosition, FranchisePositionFactor,
                                TotalScore, normPlayerScore, draft_year, Experience, 
                                lastyearcontribution, Contract.Face.Value, 
                                Guaranteed, Length.of.contract..years., Probowl, deadsamplecaps, Marginalwinsproduced))

FAdata<-FAdata[order(FAdata$Player, FAdata$FranchisePosition, FAdata$Year),]
FAdata$FAprior<-"-"
for (i in 1:nrow(FAdata)){
  player<-FAdata$Player[i]
  FAstatus<-FAdata$Freeagent[i]
  position<-FAdata$FranchisePosition[i]
  if(player==FAdata$Player[i+1] &  position==FAdata$FranchisePosition[i+1] & i<nrow(FAdata) & FAstatus=="1"){
    FAdata$FAprior[i+1]<-"1"
  }
}

FAdata$FAprior<-ifelse(FAdata$AnyTag==1, 1, FAdata$FAprior)

FAdata$Teamprior<-" "
for (i in 1:nrow(FAdata)){
  player<-FAdata$Player[i]
  FAstatus<-FAdata$Freeagent[i]
  position<-FAdata$FranchisePosition[i]
  team<-FAdata$Team[i]
  if(i<nrow(FAdata) & FAdata$FAprior[i+1]=="1"){
    FAdata$Teamprior[i+1]<-team
  }
}

FAdata<-subset(FAdata, FAdata$FAprior=="1")

scoredist<-subset(FAdata, select=c(Team, Year, TotalScore))
scoredist<-unique(scoredist)
shapiro.test(scoredist$TotalScore)

ggplot(FAdata, aes(x=YearFactor, fill=YearFactor)) +
  geom_histogram(stat="count")

##STEP 5 - GENERATE CONTRACT VALUE ESTIMATING MODELS##
consideredcontracts<-subset(FAdata, FAdata$Year>2012)

consideredcontracts<-subset(consideredcontracts, !is.na(FAdata$Contract.Face.Value))

consideredcontracts<-subset(consideredcontracts, !is.na(consideredcontracts$Length.of.contract..years.))
consideredcontracts<-subset(consideredcontracts, consideredcontracts$Year>2011)
consideredcontracts<-consideredcontracts[order(consideredcontracts$Player, consideredcontracts$Year),]

consideredcontracts$FranchisePositionFactor<-as.factor(consideredcontracts$FranchisePosition)
consideredcontracts$AnyTag1<-as.factor(consideredcontracts$AnyTag)
consideredcontracts$YearFactor<-as.factor(consideredcontracts$Year)


consideredcontractslength<-subset(consideredcontracts, select=c(Length.of.contract..years., lastyearscoring, FranchisePositionFactor, YearFactor))

predcontract1<-lm(Length.of.contract..years.~lastyearscoring+FranchisePositionFactor+YearFactor, data=consideredcontractslength)

summary(predcontract1)

ggplot(consideredcontracts, aes(x=lastyearscoring, y=Length.of.contract..years., colour=FranchisePositionFactor))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  scale_fill_discrete(name="Tag Type") 



consideredcontractsFV<-subset(consideredcontracts, select=c(Contract.Face.Value, lastyearscoring, FranchisePositionFactor, YearFactor))
#colnames(consideredcontractsFV)[3]<-"Position"

predfacevalue1<-lm(Contract.Face.Value~lastyearscoring+FranchisePositionFactor+YearFactor, data=consideredcontractsFV)

summary(predfacevalue1)

ggplot(consideredcontractsFV, aes(x=lastyearscoring, y=Contract.Face.Value, colour=FranchisePositionFactor))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  scale_x_continuous(name="Previous Year Fantasy Scoring") +
  scale_y_continuous(label=comma, name="contract Face value") 
  


consideredcontractsgteed<-subset(consideredcontracts, select=c(Guaranteed, lastyearscoring, FranchisePositionFactor, YearFactor))
#colnames(consideredcontractsgteed)[3]<-"Position"

predguaranteed1<-lm(Guaranteed~lastyearscoring+FranchisePositionFactor+YearFactor, data=consideredcontractsgteed)

summary(predguaranteed1)

ggplot(consideredcontractsgteed, aes(x=lastyearscoring, y=Guaranteed, colour=Position))+
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  scale_x_continuous(name="Previous Year Fantasy Scoring") +
  scale_y_continuous(label=comma, name="Contract Guaranteed Money")

stargazer(predcontract1, predfacevalue1, predguaranteed1)

Modeldatasetsample<-subset(ModeldatasetNO2011, ModeldatasetNO2011$Year>2012)
preddata<-subset(Modeldatasetsample, select=c(lastyearscoring, FranchisePositionFactor, YearFactor, Experience))
predcontractlength<-as.data.frame(predict(predcontract1, newdata = preddata))
colnames(predcontractlength)[1]<-"Predicted_Contract_Length"
predFV<-as.data.frame(predict(predfacevalue1, newdata=preddata))
colnames(predFV)[1]<-"Predicted_Contract_Face_Value"
predgteed<-as.data.frame(predict(predguaranteed1, newdata=preddata))
colnames(predgteed)[1]<-"Predicted_Guaranteed_Money"

FApreddata<-subset(FAdata, FAdata$Year>2012)
FApredcontractlength<-as.data.frame(predict(predcontract1, newdata=FApreddata))
colnames(FApredcontractlength)[1]<-"Predicted_Contract_Length"
FApredFV<-as.data.frame(predict(predfacevalue1, newdata=FApreddata))
colnames(FApredFV)[1]<-"Predicted_Contract_Face_Value"
FApredgteed<-as.data.frame(predict(predguaranteed1, newdata=FApreddata))
colnames(FApredgteed)[1]<-"Predicted_Guaranteed_Money"

FAdata<-cbind(FApreddata, FApredcontractlength, FApredFV, FApredgteed)

#STEP 9 - MEASURE AVAILABLE CAP SPACE##
FAdata$Capspace<-FAdata$Adjusted.cap-FAdata$deadsamplecaps

FAdata$Room<-FAdata$Capspace-FAdata$Franchise.Tag.Price
FAdatabind<-subset(FAdata, FAdata$Room<0)
FAdata<-subset(FAdata, FAdata$Room>0)

##STEP 10 - APPLY THAT BIIIIIIIIIITCH##
Modelapp<-subset(FAdata, select=c(Player, Year, YearFactor, Team, FranchisePosition, FranchisePositionFactor, lastyearscoring, seasontotal,
                                  Franchise.Tag.Price, Experience, Marginalwinsproduced, Predicted_Contract_Length, Predicted_Guaranteed_Money,
                                  Length.of.contract..years.,  Predicted_Contract_Face_Value, Guaranteed, Contract.Face.Value, AnyTag, Tag))
Modelapp<-merge(Modelapp, snapsd, by=c("Player", "Team", "Year", "FranchisePosition"), all.x=TRUE)

Modelapp$FTownteam2<-0
Modelapp$dCdAestimate<-(Modelapp$Predicted_Guaranteed_Money/Modelapp$Predicted_Contract_Length)
Modelapp$FTownteam2<-ifelse(Modelapp$Predicted_Guaranteed_Money>Modelapp$Franchise.Tag.Price, 1, Modelapp$FTownteam2)

Modelapp$rent1<-Modelapp$Predicted_Guaranteed_Money-Modelapp$Franchise.Tag.Price
Modelapp$rent2<-(Modelapp$Predicted_Contract_Face_Value/Modelapp$Predicted_Contract_Length)-Modelapp$Franchise.Tag.Price

Modelapp$AnyTagFactor<-as.factor(Modelapp$AnyTag)
Modelapp$TagFactor<-as.factor(Modelapp$Tag)

ind = createDataPartition(Modelapp$rent1, p=2/3, list=FALSE)
trainDF<-Modelapp[ind,]
testDF<-Modelapp[-ind,]


tags<-subset(Modelapp, Modelapp$AnyTag==1)

graphdata<-Modelapp
graphdata1<-subset(graphdata, select=c(rent1, AnyTagFactor))
colnames(graphdata1)[1]<-"rent"
graphdata1$Measuretype<-as.factor("Method1")
graphdata2<-subset(graphdata, select=c(rent2, AnyTagFactor))
colnames(graphdata2)[1]<-"rent"
graphdata2$Measuretype<-as.factor("Method2")
graphdata3<-rbind(graphdata1, graphdata2)

graphdata3$Tag_Used<-graphdata3$AnyTagFactor

ggplot(graphdata3)+ 
  geom_histogram(aes(x=rent, fill=Tag_Used),bindiwidth=1000) +
  scale_fill_discrete(name="Tag used") +
  scale_x_continuous(name="Estimated appropriable rents", labels = scales::comma) +
  facet_grid(Measuretype~.)

ggplot(graphdata3)+ 
  geom_boxplot(aes(x=AnyTagFactor, y=rent, fill=Tag_Used)) +
  scale_fill_discrete(name="Tag used") +
  scale_y_continuous(name="Estimated appropriable rents", labels = scales::comma) +
  scale_x_discrete((name="Tag or no tag")) +
  facet_grid(.~Measuretype)

ggplot(graphdata3)+
  geom_density(aes(x=rent, color=Tag_Used)) +
  scale_fill_discrete(name="Tag used") +
  scale_x_continuous(name="Estimated appropriable rents", labels=scales::comma) +
  facet_grid(.~Measuretype)
  
renttags<-subset(graphdata, graphdata$AnyTag==1)
rent1tags<-as.vector(subset(renttags, select=rent1))
rent2tags<-as.vector(subset(renttags, select=rent2))

rentnontag<-subset(graphdata, graphdata$AnyTag==0)
rent1nontag<-as.vector(subset(rentnontag, select=rent1))
rent2nontag<-as.vector(subset(rentnontag, select=rent2))

rent1meantest<-wilcox.test(rent1tags$rent1, rent1nontag$rent1)
rent2meantest<-wilcox.test(rent2tags$rent2, rent2nontag$rent2)

rent1meantest
rent2meantest



ggplot(Modelapp, aes(x=YearFactor, y=Franchise.Tag.Price, fill=YearFactor)) +
  geom_boxplot()


mod1logit<-glm(AnyTag~rent1, data=Modelapp, family="binomial")
summary(mod1logit)

mod1.2logit<-glm(AnyTag~rent2, data=Modelapp, family="binomial")
summary(mod1.2logit)

  prob1data<-subset(Modelapp, select=c(rent1))
  probpred1<-data.frame(predict(mod1logit, newdata=prob1data))
  colnames(probpred1)[1]<-"logit"
  probpred1$odds<-exp(probpred1$logit)
  probpred1$prob<-(probpred1$odds/(1+probpred1$odds))

  Modelprob1<-cbind(Modelapp, probpred1)
  Modelprob1$Measuretype<-as.factor("Model1")

  prob2data<-subset(Modelapp, select=c(rent2))
  probpred2<-data.frame(predict(mod1.2logit, newdata=prob2data))
  colnames(probpred2)[1]<-"logit"
  probpred2$odds<-exp(probpred2$logit)
  probpred2$prob<-(probpred2$odds/(1+probpred2$odds))

  Modelprob2<-cbind(Modelapp, probpred2)
  Modelprob2$Measuretype<-as.factor("Model2")
  
  Modelprob1$TagUsed<-Modelprob1$AnyTagFactor
  ggplot(Modelprob1, aes(rent1, prob, colour=TagUsed))+
    #stat_smooth(method="glm", family="binomial", formula=y~x, se=FALSE) +
    geom_point(aes(size=TagUsed)) +
    scale_x_continuous(name="Estimated appropriable rents", labels = scales::comma)
  
  Modelprob2$TagUsed<-Modelprob2$AnyTagFactor
  ggplot(Modelprob2, aes(rent2, prob, colour=TagUsed))+
    #stat_smooth(method="glm", family="binomial", formula=y~x, se=FALSE) +
    geom_point(aes(size=TagUsed)) +
    scale_y_continuous(limits = c(0, .75)) +
    scale_x_continuous(name="Estimated appropriable rents", labels = scales::comma)

  #Modelapp$YearFactor<-relevel(Modelapp$YearFactor, ref=2)

mod2logit<-glm(AnyTag~rent1+YearFactor, data=Modelapp, family="binomial")
summary(mod2logit)
mod2.2logit<-glm(AnyTag~rent2+YearFactor, data=Modelapp, family="binomial")
summary(mod2.2logit)

mod3logit<-glm(AnyTag~rent1+YearFactor+Experience, data=Modelapp, family="binomial")
summary(mod3logit)
mod3.2logit<-glm(AnyTag~rent2+YearFactor+Experience, data=Modelapp, family="binomial")
summary(mod3.2logit)

mod4logit<-glm(AnyTag~rent1+Experience+YearFactor+FranchisePositionFactor, data=Modelapp, family="binomial")
summary(mod4logit)
mod4.2logit<-glm(AnyTag~rent2+Experience+YearFactor+FranchisePositionFactor, data=Modelapp, family="binomial")
summary(mod4.2logit)

  prob4data<-subset(Modelapp, select=c(rent1, Experience, YearFactor, FranchisePositionFactor))
  probpred4<-data.frame(predict(mod4logit, newdata=prob4data))
  colnames(probpred4)[1]<-"logit"
  probpred4$odds<-exp(probpred4$logit)
  probpred4$prob<-(probpred4$odds/(1+probpred4$odds))

  Modelprob4<-cbind(Modelapp, probpred4)
  Modelprob4$Measuretype<-as.factor("Model1")

  prob4.2data<-subset(Modelapp, select=c(rent2, Experience, YearFactor, FranchisePositionFactor))
  probpred4.2<-data.frame(predict(mod4.2logit, newdata=prob4.2data))
  colnames(probpred4.2)[1]<-"logit"
  probpred4.2$odds<-exp(probpred4.2$logit)
  probpred4.2$prob<-(probpred4.2$odds/(1+probpred4.2$odds))

  Modelprob4.2<-cbind(Modelapp, probpred4.2)
  Modelprob4.2$Measuretype<-as.factor("Model2")

  ggplot(Modelprob4, aes(rent1, prob, colour=AnyTagFactor))+
    #stat_smooth(method="glm", family="binomial", formula=y~x, se=FALSE) +
    geom_point(aes(size=AnyTagFactor)) +
    scale_x_continuous(name="Estimated appropriable rents", labels = scales::comma)

  ggplot(Modelprob4.2, aes(rent2, prob, colour=AnyTagFactor))+
    #stat_smooth(method="glm", family="binomial", formula=y~x, se=FALSE) +
    geom_point(aes(size=AnyTagFactor)) +
    scale_x_continuous(name="Estimated appropriable rents", labels = scales::comma)


mod5logit<-glm(AnyTag~rent1+Experience+Snapsd+YearFactor+FranchisePositionFactor, data=Modelapp, family="binomial")
summary(mod5logit)
mod5.2logit<-glm(AnyTag~rent2+Experience+Snapsd+YearFactor+FranchisePositionFactor, data=Modelapp, family="binomial")
summary(mod5.2logit)



stargazer(mod1logit, mod2logit, mod3logit, mod4logit, mod5logit, digits=6)

stargazer(mod1.2logit, mod2.2logit, mod3.2logit, mod4.2logit, mod5.2logit, digits=6)

modcoll<-lm(rent1~experience, data=Modelapp)
summary(modcolltest)


##STEP 10 - COMPARE MEAN RENTS ACROSS FRANCHISE TAGS##
Tags<-subset(Modelapp, Modelapp$AnyTag==1)
Tags<-subset(Tags, !is.na(Tags$seasontotal))
Tags<-unique(Tags)

ggplot(data=Tags, aes(x=TagFactor, y=rent1, fill=TagFactor)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(name="Estimated appropriable rents", labels = scales::comma) +
  scale_x_discrete((name="Franchise Tag Outcome")) 


ggplot(data=Tags, aes(x=TagFactor, y=rent2, fill=TagFactor)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(name="Estimated appropriable rents", labels = scales::comma) +
  scale_x_discrete((name="Franchise Tag Outcome")) 
  

meandif<-aov(formula = rent1~TagFactor, data = Tags)
summary(meandif)

Tags2<-subset(Tags, Tags$TagFactor=="FranchiseTag" | Tags$TagFactor=="NEFranchiseTag" | Tags$TagFactor=="NEFranchiseTagDeclined" |
                Tags$TagFactor=="FranchiseTagRescinded")
Tags2$GenericTag<-NA
Tags2$GenericTag<-as.factor(ifelse((Tags2$TagFactor=="NEFranchiseTagDeclined" | Tags2$TagFactor=="FranchiseTagRescinded"), "NO", "YES"))
Tags2$Playedseason<-as.factor(Tags2$GenericTag)

ggplot(data=Tags2, aes(x=Playedseason, y=rent1, fill=Playedseason)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(name="Estimated appropriable rents", labels = scales::comma) +
  scale_x_discrete((name="Franchise Tag Outcome")) 

meandif2<-aov(formula = rent1~Playedseason, data = Tags2)
summary(meandif2)

pander(meandif2)

played<-subset(Tags2, Tags2$Playedseason=="YES")
noplay<-subset(Tags2, Tags2$Playedseason=="NO")

meantest2<-wilcox.test(played$rent1, noplay$rent1)

meantest2

noplay$ID<-paste(noplay$Player, noplay$Year)
noplaymelt1<-subset(noplay, select=c(ID, Predicted_Contract_Face_Value, Contract.Face.Value))
noplaymelt1<-subset(noplaymelt1, !is.na(noplaymelt1$Contract.Face.Value))
noplaymelt1<-melt(noplaymelt1, ID="ID")

ggplot(noplaymelt1, aes(x=ID, y=value, group=variable, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels = scales::comma) 
  
noplay$Face_Value_Difference<-noplay$Predicted_Contract_Face_Value-noplay$Contract.Face.Value

noplaymelt2<-subset(noplay, select=c(ID, Predicted_Guaranteed_Money, Guaranteed))
noplaymelt2<-subset(noplaymelt2, !is.na(noplaymelt2$Guaranteed))
noplaymelt2<-melt(noplaymelt2, ID="ID")

ggplot(noplaymelt2, aes(x=ID, y=value, group=variable, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels= scales::comma)

noplay$Guaranteed_Difference<-noplay$Predicted_Guaranteed_Money-noplay$Guaranteed

noplaymelt3<-subset(noplay, select=c(ID, Face_Value_Difference, Guaranteed_Difference))
noplaymelt3<-subset(noplaymelt3, !is.na(noplaymelt3$Face_Value_Difference))
noplaymelt3<-melt(noplaymelt3, ID="ID")

ggplot(noplaymelt3, aes(x=ID, y=value, group=variable, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels=scales::comma)

##STEP 11 - MEASURE QUALITY OF AVAILABLE FREE-AGENT REPLACEMENTS##
replacements<-FAdata

replacements<-replacements[order(replacements$FranchisePosition, replacements$Year),]

replacementagg<-0
maxreplacement<-0
n<-0
replacements$aggreplacementscore<-NA
replacements$maxreplacementscore<-NA
replacements$avgreplacementscore<-NA
replacements$numreplacements<-NA
for (i in 1:nrow(replacements)){
  replacementscore<-replacements$lastyearscoring[i]
  year<-replacements$Year[i]
  if (year==replacements$Year[i+1] & i<nrow(replacements)){
    replacementagg<-replacementscore+replacementagg
    if (replacementscore>=maxreplacement){
      maxreplacement<-replacementscore
    }
    n=n+1
  } else if (year!=replacements$Year[i+1] & i<nrow(replacements)){
    replacementagg<-replacementscore+replacementagg
    if (replacementscore>=maxreplacement){
      maxreplacement<-replacementscore
    }
    n=n+1
    replacements$aggreplacementscore[i]<-replacementagg
    replacements$maxreplacementscore[i]<-maxreplacement
    replacements$avgreplacementscore[i]<-(replacementagg/n)
    replacements$numreplacements[i]<-n
    replacementagg<-0
    maxreplacement<-0
    avgreplacementscore<-0
    numreplacements<-0
    n<-0
  } else {
    replacementagg<-replacementscore+replacementagg
    if (replacementscore>=maxreplacement){
      maxreplacement<-replacementscore
    }
    n=n+1
    replacements$aggreplacementscore[i]<-replacementagg
    replacements$maxreplacementscore[i]<-maxreplacement
    replacements$avgreplacementscore[i]<-(replacementagg/n)
    replacements$numreplacements[i]<-n
  }
}

replacements<-subset(replacements, select=c(Year, FranchisePosition, maxreplacementscore, numreplacements, avgreplacementscore))
replacements<-subset(replacements, !is.na(maxreplacementscore))

FAdata<-merge(FAdata, replacements, by=c("Year", "FranchisePosition"), allx=TRUE)
















###REGRESSION OF FANTASY SCORES ON WINS - MODEL#####
ModeldatasetNO2011<-subset(Modeldataset, Modeldataset$Year!="2011")

plot(ModeldatasetNO2011$TotalScore, ModeldatasetNO2011$W)
abline(lm(ModeldatasetNO2011$W~ModeldatasetNO2011$TotalScore))

winmodel<-lm(W~TotalScore, data=ModeldatasetNO2011)
#stargazer(winmodel)
summary(winmodel)

new<-data.frame(TotalScore = ModeldatasetNO2011$TotalScore)

newdata<-data.frame(predict.lm(winmodel, newdata=new, interval='confidence'))
ModeldatasetNO2011<-cbind(ModeldatasetNO2011, newdata)

ModeldatasetNO2011$Predwins<-predict.lm(winmodel, newdata=new, interval='confidence')
ModeldatasetNO2011$Residuals<-resid(winmodel)

ggplot(ModeldatasetNO2011, aes(x=Residuals)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

ggplot(ModeldatasetNO2011, aes(x=Residuals, y=NonPlayerOutput)) +
  geom_point()


              

##STEP 3 - MEASURE VALUE OVER REPLACEMENT##
VAR<-subset(fulldatav2, select=c(Player, Team, FranchisePosition, Year, Week, Points, W, Total.Snaps, Injury.listing, IR, seasonsnaps, Started, Line, 
                                 Over.Odds, Under.Odds))
VAR$Total.Snaps<-ifelse(is.na(VAR$Total.Snaps), 0, VAR$Total.Snaps)
VAR$Started<-ifelse(is.na(VAR$Started), "NO", VAR$Started)
VAR$Started<-as.factor(VAR$Started)
VAR$Injury.listing<-ifelse(is.na(VAR$Injury.listing), "None", VAR$Injury.listing)
VAR<-subset(VAR, VAR$Year>=2012)
VAR<-VAR[order(VAR$Team, VAR$FranchisePosition, VAR$Year, VAR$Player,VAR$Week),]
VAR$Healthyavg<-NA
VAR$Healthysnaps<-0
VAR$Injured<-0
VAR$Snapsd<-0
VAR$IR<-0

VAR$Injured<-ifelse(VAR$Injury.listing=="Questionable" | VAR$Injury.listing=="Doubtful" | VAR$Injury.listing=="Out" | 
                      VAR$Injury.listing=="Injured Reserve", 1, VAR$Injured)

seasonhealthysnaps<-0
seasoninjuredsnaps<-0
h<-0
u<-0
for(i in 1:nrow(VAR)){
  week<-VAR$Week[i]
  player<-VAR$Player[i]
  injury<-VAR$Injury.listing[i]
  snaps<-VAR$Total.Snaps[i]
  if(player==VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==0){
    seasonhealthysnaps<-seasonhealthysnaps + snaps
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    h<-h+1
  } else if (player==VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==1){
    u<-u+1
  } else if (player!=VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==0){
    seasonhealthysnaps<-seasonhealthysnaps+snaps
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    h<-h+1
    VAR$Healthyavg[i]<-(seasonhealthysnaps/h)
    VAR$Snapsd[i]<-sd(VAR$Total.Snaps[(i-(h+u)+1):i])
    h<-0
    u<-0
    seasonhealthysnaps<-0
  } else if (player!=VAR$Player[i+1] & i<nrow(VAR) & VAR$Injured[i]==1){
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    u<-u+1
    VAR$Healthyavg[i]<-(seasonhealthysnaps/h)
    VAR$Snapsd[i]<-sd(VAR$Total.Snaps[(i-(h+u)+1):i])
    h<-0
    u<-0
    seasonhealthysnaps<-0
  } else if (i==nrow(VAR)){
    VAR$Healthyavg[i]<-(seasonhealthysnaps/h)
    VAR$Healthysnaps[i]<-seasonhealthysnaps
    VAR$Snapsd[i]<-sd(VAR$Total.Snaps[(i-(h+u)+1):i])
  }
}

healthyavg<-subset(VAR, !is.na(VAR$Healthyavg))
healthyavg<-subset(healthyavg, select=c(Player, Team, Year, FranchisePosition, Healthyavg))
VAR<-subset(VAR, select=-c(Healthyavg))
VAR<-merge(VAR, healthyavg, by=c("Player", "Team", "Year", "FranchisePosition"))

snapsd<-subset(VAR, !is.na(VAR$Healthyavg))
snapsd<-subset(snapsd, select=c(Player, Team, Year, FranchisePosition, Snapsd, Healthyavg))

VAR<-VAR[order(VAR$Team, VAR$Year, VAR$FranchisePosition, VAR$Player, VAR$Week),]

VAR$Missedfrominjury<-NA
VAR$Totalinjmissed<-NA
for (i in 1:nrow(VAR)){
  started<-VAR$Started[i]
  injured<-VAR$Injured[i]
  player<-VAR$Player[i]
  snaps<-VAR$Total.Snaps[i]
  healthyavg<-VAR$Healthyavg[i]
  if (player==VAR$Player[i+1] & i<nrow(VAR) & started=="YES"  & VAR$Started[i+1]=="NO" & VAR$Injured[i+1]==1){
    injmissed<-(snaps-VAR$Total.Snaps[i+1])
    totalinjmissed<-injmissed+totalinjmissed
    VAR$Missedfrominjury[i+1]<-injmissed
  } else if (player==VAR$Player[i+1] & i<nrow(VAR) & started=="YES"  & VAR$Started[i+1]=="YES" & VAR$Injured[i+1]==1 & VAR$Total.Snaps[i+1]<snaps){
    injmissed<-(snaps-VAR$Total.Snaps[i+1])
    totalinjmissed<-injmissed+totalinjmissed
    VAR$Missedfrominjury[i+1]<-injmissed
  } else if (player==VAR$Player[i+1] & i<nrow(VAR) & started=="YES"  & VAR$Started[i+1]=="NO" & VAR$Injury.listing[i+1]=="Injured Reserve"){
    injmissed<-healthyavg
    totalinjmissed<-injmissed+totalinjmissed
    VAR$Missedfrominjury[i+1]<-injmissed
  } else if (player==VAR$Player[i+1] & i<nrow(VAR) & started=="NO"  & VAR$Started[i+1]=="NO" & VAR$Injury.listing[i+1]=="Injured Reserve"){
    injmissed<-healthyavg
    totalinjmissed<-injmissed+totalinjmissed
    VAR$Missedfrominjury[i+1]<-injmissed
  } else if (player!=VAR$Player[i+1] & i<nrow(VAR) & VAR$Injury.listing[i]=="Injured Reserve"){
    injmissed<-healthyavg
    totalinjmissed<-injmissed+totalinjmissed
    VAR$Missedfrominjury[i]<-injmissed
    VAR$Totalinjmissed[i]<-totalinjmissed
    totalinjmissed<-0
  } else if (player!=VAR$Player[i+1] & i<nrow(VAR) & VAR$Injury.listing[i]!="Injured Reserve"){
    VAR$Totalinjmissed[i]<-totalinjmissed
    totalinjmissed<-0
  } else if (i==nrow(VAR)){
    VAR$Missedfrominjury[i]<-injmissed
    VAR$Totalinjmissed[i]<-totalinjmissed
  }
}



startedsum<-0
totalsum<-0
VAR$Startersnaps<-NA
VAR$Totalpositionsnaps<-NA
VAR$Numobs<-NA
n<-0
for (i in 1:nrow(VAR)){
  started<-VAR$Started[i]
  year<-VAR$Year[i]
  snaps<-VAR$Total.Snaps[i]
  position<-VAR$FranchisePosition[i]
  if(position==VAR$FranchisePosition[i+1] & started=="YES" & i<nrow(VAR)){
    startedsum<-startedsum+snaps
    totalsum<-totalsum+snaps
    VAR$Totalpositionsnaps[i]<-totalsum
    n<-n+1
  } else if(position==VAR$FranchisePosition[i+1] & started=="NO" & i<nrow(VAR)){
    totalsum<-totalsum+snaps
    VAR$Totalpositionsnaps[i]<-totalsum
  } else if(position!=VAR$FranchisePosition[i+1] & started=="YES" & i<nrow(VAR)){
    startedsum<-startedsum+snaps
    totalsum<-totalsum+snaps
    VAR$Startersnaps[i]<-startedsum
    VAR$Totalpositionsnaps[i]<-totalsum
    VAR$Numobs[i]<-n
    startedsum<-0
    totalsum<-0
    n<-0
  } else if(position!=VAR$FranchisePosition[i+1] & started=="NO" & i<nrow(VAR)){
    totalsum<-totalsum+snaps
    VAR$Startersnaps[i]<-startedsum
    VAR$Totalpositionsnaps[i]<-totalsum
    startedsum<-0
    totalsum<-0
  } else if (i==nrow(VAR) & started=="YES"){
    startedsum<-startedsum+snaps
    totalsum<-totalsum+snaps
    VAR$Startersnaps[i]<-startedsum
    VAR$Totalpositionsnaps[i]<-totalsum
    VAR$Numobs[i]<-n
  } else if (i==nrow(VAR) & started=="NO"){
    totalsum<-totalsum+snaps
    VAR$Startersnaps[i]<-startedsum
    VAR$Totalpositionsnaps[i]<-totalsum
  }
}

starters<-subset(VAR, !is.na(VAR$Startersnaps))
starters<-subset(starters, select=c(Team, Year, FranchisePosition, Startersnaps, Totalpositionsnaps, Numobs, Line, Over.Odds, Under.Odds))
starters<-starters[order(starters$FranchisePosition, starters$Year),]

starters$startersnappct<-NA

startersnapsum<-0
totalsnapsum<-0
for (i in 1:nrow(starters)){
  year<-starters$Year[i]
  teamstartersnaps<-starters$Startersnaps[i]
  teamtotalsnaps<-starters$Totalpositionsnaps[i]
  if (year==starters$Year[i+1] & i<nrow(starters)){
    startersnapsum<-startersnapsum+teamstartersnaps
    totalsnapsum<-totalsnapsum+teamtotalsnaps
  } else if (year!=starters$Year[i+1] & i<nrow(starters)){
    startersnapsum<-startersnapsum+teamstartersnaps
    totalsnapsum<-totalsnapsum+teamtotalsnaps
    starters$startersnappct[i]<-startersnapsum/totalsnapsum
    startersnapsum<-0
    totalsnapsum<-0
  } else if (i==nrow(starters)){
    startersnapsum<-startersnapsum+teamstartersnaps
    totalsnapsum<-totalsnapsum+teamtotalsnaps
    starters$startersnappct[i]<-startersnapsum/totalsnapsum
  }
}

startersstripped<-subset(starters, !is.na(starters$startersnappct))

startersstripped$fullsampleavg<-NA
fullsamplepct<-0
for(i in 1:nrow(startersstripped)){
  yearpct<-startersstripped$startersnappct[i]
  pos<-startersstripped$FranchisePosition[i]
  if (pos==startersstripped$FranchisePosition[i+1] & i<nrow(startersstripped)){
    fullsamplepct<-yearpct+fullsamplepct
  } else if (pos!=startersstripped$FranchisePosition[i+1] & i<nrow(startersstripped)){
    fullsamplepct<-yearpct+fullsamplepct
    startersstripped$fullsamapleavg[i]<-fullsamplepct/5
    fullsamplepct<-0
  } else if (i==nrow(startersstripped)){
    fullsamplepct<-yearpct+fullsamplepct
    startersstripped$fullsamapleavg[i]<-fullsamplepct/5
  }
}

missed<-subset(VAR, !is.na(VAR$Totalinjmissed))
missed<-subset(missed, select=c(Team,FranchisePosition, Year, W, Line, Totalinjmissed, Totalpositionsnaps))
missed<-missed[order(missed$FranchisePosition, missed$Year),]

missed$Positiontotalmissed<-NA
aggmissed<-0
for(i in 1:nrow(missed)){
  team<-missed$Team[i]
  missedsnaps<-missed$Totalinjmissed[i]
  if(team==missed$Team[i+1] & i<nrow(missed)){
    aggmissed<-(missedsnaps+aggmissed)
  } else if (team!=missed$Team[i+1] & i<nrow(missed)){
    aggmissed<-missedsnaps+aggmissed
    missed$Positiontotalmissed[i]<-aggmissed
    aggmissed<-0
  } else if (i==nrow(missed)){
    aggmissed<-missedsnaps+aggmissed
    missed$Positiontotalmissed[i]<-aggmissed
  }
}

missed<-subset(missed, !is.na(missed$Positiontotalmissed))
missed$pctmissed<-(missed$Positiontotalmissed/missed$Totalpositionsnaps)

ggplot(missed, aes(x=pctmissed, y=W, color=factor(FranchisePosition))) +
  geom_jitter() +
  geom_smooth(method="lm", se=FALSE)

##COOL VISUALS FOR POWERPOINT / DEFENSE##
VARvisual<-subset(VAR, VAR$Team=="WAS" & VAR$Year==2015 & VAR$FranchisePosition=="DT")
VARvisual<-subset(VARvisual, VARvisual$seasonsnaps>30)
VARvisual$PlayerFactor<-as.factor(VARvisual$Player)

VARvisualWL<-
  
  ggplot(VARvisual, aes(x=Week, y=Total.Snaps, color=PlayerFactor)) +
  geom_point() +
  geom_line(size=1) +
  annotate("rect", fill= "red", alpha=.5,
           xmin=1.5, xmax=2.5,
           ymin=-Inf, ymax=Inf)