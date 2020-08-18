library(tidyverse)
library(ggplot2)
library(lme4)


########
######
#Study 1 
######
######


reasonsr.w<-read.csv("reasonsr2.csv")
length(reasonsr.w$subjectcode) #n=150

reasonsr<-reasonsr.w %>%
  gather(question.code,answer,-c(1:3,52:59),na.rm=TRUE) 
reasonsr$question.code = factor(reasonsr$question.code)
#character vectors to label the questions
question.list<-c(rep(c("outcome","unfair","universalization","harm"),12)) 
context.list<-c(rep("hiking",4),rep("museum",4),rep("story.time",4),rep("extra.snack",4),rep("art.project",4),rep("homework",4),rep("stickers",4),
                rep("cleanup",4),rep("cookies",4),rep("punch",4),rep("insult",4),rep("allergy",4))
condition.list<-c(rep("universalization",12),rep("outcome",12),rep("unfair",12),rep("harm",12))
#adds the labels to the long-form data
reasonsr<-reasonsr %>%
  mutate(
    question=factor(question.list[question.code]),
    context=context.list[question.code],
    condition=condition.list[question.code]
  )
reasonsr<-reasonsr[-c(12)]


#exclusion flags
reasonsr$flag<-NA
reasonsr$flag[reasonsr$check1==1]<-1
reasonsr$flag[reasonsr$check2==1]<-1
reasonsr$flag[reasonsr$check3==1]<-1
reasonsr$flag[reasonsr$check4==1]<-1
reasonsr$flag[reasonsr$check5==1]<-1
reasonsr$flag[reasonsr$check6==1]<-1
reasonsr$flag[reasonsr$check7!="I am paying attention"]<-1

#take out excluded subjects
reasonsr<-reasonsr[is.na(reasonsr$flag),]
reasonsr<-reasonsr[-c(4:10,16)] #data set is now clean

#get each subject's mean answer for each condition (across all 3 stories) for each question
reasonsr.c<-reasonsr %>% #collapsed form
  group_by(subjectcode,question,condition)%>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T)
  )

#summarize collapsed data
reasonsr.c.sum <- reasonsr.c %>% 
  group_by(question,condition) %>%
  summarize(
    n = length(mean),
    mean2 = mean(mean, na.rm = T),
    se = sqrt((mean2*(1-mean2))/n)
  )



#graph collapsed data
reasonsr.c.sum %>%
  ggplot(aes(y=mean2, x=condition, fill=question)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean2-se, ymax=mean2+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Condition")+
  ylab("P (convincing)")+
  #scale_fill_manual(name = "Condition", 
  #labels = c("High Interest", "Low Interest"),
  #values=c('blue','red'))+
  #scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 20))+
  scale_fill_brewer(palette="Pastel1")

#summarize data (not collapsed across contexts)
reasonsr.sum <- reasonsr %>% 
  group_by(question,condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph
reasonsr.sum %>%
  ggplot(aes(y=mean, x=condition, fill=question)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Condition")+
  ylab("P (convincing)")+
  #scale_fill_manual(name = "Condition", 
  #labels = c("High Interest", "Low Interest"),
  #values=c('blue','red'))+
  #scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#summarize data by context
reasonsr.sum2 <- reasonsr %>% 
  group_by(question,context) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph
reasonsr.sum2 %>%
  ggplot(aes(y=mean, x=context, fill=question)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Context")+
  ylab("P (convincing)")+
  #scale_fill_manual(name = "Condition", 
  #labels = c("High Interest", "Low Interest"),
  #values=c('blue','red'))+
  #scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#what percentage of subjects say that no answer applies?  do any say this?
#get each subject's mean answer for each context (across all 4 questions) 
reasonsr.c2<-reasonsr %>% #collapsed form
  group_by(subjectcode,context)%>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T)
  )

length(reasonsr.c2$subjectcode) #n=1560
reasonsr.c2$mean<-factor(reasonsr.c2$mean)

table(reasonsr.c2$mean)
#   0 0.25  0.5 0.75    1 
#  133  569  445  313  100 

#> 133/1560
#[1] 0.08525641
#> 569/1560
#[1] 0.3647436
#> 445/1560
#[1] 0.2852564
#> 313/1560
#[1] 0.200641
#> 100/1560
#[1] 0.06410256

#summarize collapsed data
reasonsr.c.sum <- reasonsr.c %>% 
  group_by(question,condition) %>%
  summarize(
    n = length(mean),
    mean2 = mean(mean, na.rm = T),
    se = sqrt((mean2*(1-mean2))/n)
  )


######
#analysis for reason-matching to go in the paper
######

#chi-square tests

reasonsr3<-reasonsr[reasonsr$condition=="universalization",]
reasonsr3<-reasonsr3[reasonsr3$question=="universalization" | reasonsr3$question=="outcome",]

chisq.test(reasonsr3$answer,reasonsr3$question,correct=FALSE) 
#X-squared = 369.42, df = 1, p-value < 2.2e-16

reasonsr4<-reasonsr[reasonsr$condition=="universalization",]
reasonsr4<-reasonsr4[reasonsr4$question=="universalization" | reasonsr4$question=="harm",]

chisq.test(reasonsr4$answer,reasonsr4$question,correct=FALSE) 
#X-squared = 257.48, df = 1, p-value < 2.2e-16

reasonsr5<-reasonsr[reasonsr$condition=="universalization",]
reasonsr5<-reasonsr5[reasonsr5$question=="universalization" | reasonsr5$question=="unfair",]

chisq.test(reasonsr5$answer,reasonsr5$question,correct=FALSE) 
#X-squared = 163.88, df = 1, p-value < 2.2e-16

reasonsr7<-reasonsr[reasonsr$condition=="harm" | reasonsr$condition=="universalization",]
reasonsr7<-reasonsr7[reasonsr7$question=="universalization",]
chisq.test(reasonsr7$answer,reasonsr7$condition,correct=FALSE) 
#X-squared = 56.073, df = 1, p-value = 6.982e-14

reasonsr8<-reasonsr[reasonsr$condition=="unfair" | reasonsr$condition=="universalization",]
reasonsr8<-reasonsr8[reasonsr8$question=="universalization",]
chisq.test(reasonsr8$answer,reasonsr8$condition,correct=FALSE) 
#X-squared = 180.19, df = 1, p-value < 2.2e-16

reasonsr9<-reasonsr[reasonsr$condition=="outcome" | reasonsr$condition=="universalization",]
reasonsr9<-reasonsr9[reasonsr9$question=="universalization",]
chisq.test(reasonsr9$answer,reasonsr9$condition,correct=FALSE) 
#X-squared = 153.49, df = 1, p-value < 2.2e-16

reasonsr10<-reasonsr[reasonsr$condition=="outcome" & reasonsr$question=="outcome",]
binom.test(sum(reasonsr10$answer),length(reasonsr10$answer),p=.5) 
#number of successes = 290, number of trials = 390, p-value < 2.2e-16
reasonsr11<-reasonsr[reasonsr$condition=="harm" & reasonsr$question=="harm",]
binom.test(sum(reasonsr11$answer),length(reasonsr11$answer),p=.5) 
#number of successes = 350, number of trials = 390, p-value < 2.2e-16
reasonsr12<-reasonsr[reasonsr$condition=="unfair" & reasonsr$question=="unfair",]
binom.test(sum(reasonsr12$answer),length(reasonsr12$answer),p=.5) 
#number of successes = 338, number of trials = 390, p-value < 2.2e-16



########
######
#Study 1 Replication (reported in supplement)
#slightly different instructions to participants
######
######


reasons.w<-read.csv("reasons1.csv")
length(reasons.w$subjectcode) #n=150

reasons<-reasons.w %>%
  gather(question.code,answer,-c(1:3,52:59),na.rm=TRUE) 
reasons$question.code = factor(reasons$question.code)
#character vectors to label the questions
question.list<-c(rep(c("outcome","unfair","universalization","harm"),12)) 
context.list<-c(rep("hiking",4),rep("museum",4),rep("story.time",4),rep("extra.snack",4),rep("art.project",4),rep("homework",4),rep("stickers",4),
                rep("cleanup",4),rep("cookies",4),rep("punch",4),rep("insult",4),rep("allergy",4))
condition.list<-c(rep("universalization",12),rep("outcome",12),rep("unfair",12),rep("harm",12))
#adds the labels to the long-form data
reasons<-reasons %>%
  mutate(
    question=factor(question.list[question.code]),
    context=context.list[question.code],
    condition=condition.list[question.code]
  )
reasons<-reasons[-c(12)]


#exclusion flags
reasons$flag<-NA
reasons$flag[reasons$check1==1]<-1
reasons$flag[reasons$check2==1]<-1
reasons$flag[reasons$check3==1]<-1
reasons$flag[reasons$check4==1]<-1
reasons$flag[reasons$check5==1]<-1
reasons$flag[reasons$check6==1]<-1
reasons$flag[reasons$check7!="I am paying attention"]<-1

#take out excluded subjects
reasons<-reasons[is.na(reasons$flag),]
reasons<-reasons[-c(4:10,16)] #data set is now clean

#get each subject's mean answer for each condition (across all 3 stories) for each question
reasons.c<-reasons %>% #collapsed form
  group_by(subjectcode,question,condition)%>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T)
  )

#summarize collapsed data
reasons.c.sum <- reasons.c %>% 
  group_by(question,condition) %>%
  summarize(
    n = length(mean),
    mean2 = mean(mean, na.rm = T),
    se = sqrt((mean2*(1-mean2))/n)
  )



#graph collapsed data
reasons.c.sum %>%
  ggplot(aes(y=mean2, x=condition, fill=question)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean2-se, ymax=mean2+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Condition")+
  ylab("P (convincing)")+
  #scale_fill_manual(name = "Condition", 
  #labels = c("High Interest", "Low Interest"),
  #values=c('blue','red'))+
  #scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#summarize data (not collapsed across contexts)
reasons.sum <- reasons %>% 
  group_by(question,condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph
reasons.sum %>%
  ggplot(aes(y=mean, x=condition, fill=question)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Condition")+
  ylab("P (convincing)")+
  #scale_fill_manual(name = "Condition", 
  #labels = c("High Interest", "Low Interest"),
  #values=c('blue','red'))+
  #scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#summarize data by context
reasons.sum2 <- reasons %>% 
  group_by(question,context) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph
reasons.sum2 %>%
  ggplot(aes(y=mean, x=context, fill=question)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Context")+
  ylab("P (convincing)")+
  #scale_fill_manual(name = "Condition", 
  #labels = c("High Interest", "Low Interest"),
  #values=c('blue','red'))+
  #scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#what percentage of subjects say that no answer applies?  do any say this?
#get each subject's mean answer for each context (across all 4 questions) 
reasons.c2<-reasons %>% #collapsed form
  group_by(subjectcode,context)%>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T)
  )

length(reasons.c2$subjectcode) #n=1560
reasons.c2$mean<-factor(reasons.c2$mean)

table(reasons.c2$mean)
#   0 0.25  0.5 0.75    1 
#  109  571  494  183  91 



######
#analysis for reason-matching to go in the paper
######

#chi-square tests

reasons3<-reasons[reasons$condition=="universalization",]
reasons3<-reasons3[reasons3$question=="universalization" | reasons3$question=="outcome",]

chisq.test(reasons3$answer,reasons3$question,correct=FALSE) 
#X-squared = 259.77, df = 1, p-value < 2.2e-16

reasons4<-reasons[reasons$condition=="universalization",]
reasons4<-reasons4[reasons4$question=="universalization" | reasons4$question=="harm",]

chisq.test(reasons4$answer,reasons4$question,correct=FALSE) 
#X-squared = 190.61, df = 1, p-value < 2.2e-16

reasons5<-reasons[reasons$condition=="universalization",]
reasons5<-reasons5[reasons5$question=="universalization" | reasons5$question=="unfair",]

chisq.test(reasons5$answer,reasons5$question,correct=FALSE) 
#X-squared = 325.98, df = 3, p-value < 2.2e-16

reasons7<-reasons[reasons$condition=="harm" | reasons$condition=="universalization",]
reasons7<-reasons7[reasons7$question=="universalization",]
chisq.test(reasons7$answer,reasons7$condition,correct=FALSE) 
#X-squared = 64.839, df = 1, p-value = 8.126e-1

reasons8<-reasons[reasons$condition=="unfair" | reasons$condition=="universalization",]
reasons8<-reasons8[reasons8$question=="universalization",]
chisq.test(reasons8$answer,reasons8$condition,correct=FALSE) 
#X-squared = 126.38, df = 1, p-value < 2.2e-16

reasons9<-reasons[reasons$condition=="outcome" | reasons$condition=="universalization",]
reasons9<-reasons9[reasons9$question=="universalization",]
chisq.test(reasons9$answer,reasons9$condition,correct=FALSE) 
#X-squared = 109.71, df = 1, p-value < 2.2e-16

reasons10<-reasons[reasons$condition=="outcome" & reasons$question=="outcome",]
binom.test(sum(reasons10$answer),length(reasons10$answer),p=.5) 
#number of successes = 266, number of trials = 387, p-value = 1.289e-13
reasons11<-reasons[reasons$condition=="harm" & reasons$question=="harm",]
binom.test(sum(reasons11$answer),length(reasons11$answer),p=.5) 
#number of successes = 317, number of trials = 387, p-value < 2.2e-16
reasons12<-reasons[reasons$condition=="unfair" & reasons$question=="unfair",]
binom.test(sum(reasons12$answer),length(reasons12$answer),p=.5) 
#number of successes = 321, number of trials = 387, p-value < 2.2e-1


########
######
#Study 2a
#high interest, low interest; multiple contexts
######
######

data8.w<-read.csv("contexts.csv")
length(data8.w$subjectcode) #n=1002
data8.w<-head(data8.w,-2) #collected 2 extra subjects, so removed the last 2 rows

data8<-data8.w %>%
  gather(question.code,answer,-c(1:3,74:75),na.rm=TRUE) 
data8$question.code = factor(data8$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","moral","rule","outcome"),10)) 
ip.list<-c(rep(c(rep("high",7),rep("low",7)),5))
context.list<-c(rep("clams",14),rep("fish",14),rep("rabbits",14),rep("birds",14),rep("mushrooms",14))
#adds the labels to the long-form data
data8<-data8 %>%
  mutate(
    question=factor(question.list[question.code]),
    context=context.list[question.code],
    ip=ip.list[question.code]
  )
data8<-data8[-c(6)]

#exclusion flags
data8$flag<-NA
data8$flag[data8$ip=="low" & data8$question=="interest" & data8$answer!=0]<-1
data8$flag[data8$ip=="high" & data8$question=="interest" & data8$answer!=20]<-1
data8$flag[data8$ip=="high" & data8$question=="interest" & data8$answer==19]<-NA
data8$flag[data8$question=="fishermen"]<-1
data8$flag[data8$question=="fishermen" & data8$answer==19]<-NA
data8$flag[data8$question=="fishermen" & data8$answer==20]<-NA
data8$flag[data8$question=="fishermen" & data8$answer==21]<-NA
data8$flag[data8$question=="usage" & data8$answer!=0]<-1
data8$flag[data8$question=="outcome" & data8$answer!=3]<-1

#summarize data to flag subjects to exclude
data8.exclude <- data8 %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
data8<-join(data8,data8.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)
#take out excluded subjects
data8<-data8[data8$exclude==0,]
data8<-data8[-c(10:11)] #data set is now clean

#summarize data
data8.sum <- data8 %>% 
  group_by(ip,question) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph for paper
data8.sum %>%
  filter(question=="moral")%>%
  ggplot(aes(y=mean, x=ip, fill=ip)) + 
  geom_bar(stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  geom_text(x = 1.5, y = .9, label = "***", size=10)+
  geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Interested Parties")+
  ylab("P (morally acceptable)")+
  scale_fill_manual("Condition",
                    values=c("high" = "#B3CDE3","low" = "#FBB4AE"))+
  #scale_x_discrete(labels=c("High Interest", "Low Interest"),
  #values=c('skyblue3','lightcoral'))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#summarize data
data8.sum2 <- data8 %>% 
  group_by(ip,question, context) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph
data8.sum2 %>%
  filter(question=="moral")%>%
  ggplot(aes(y=mean, x=context, fill=ip)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Interested Parties")+
  ylab("P (morally acceptable)")+
  scale_fill_manual(name = "Condition", 
                    labels = c("High Interest", "Low Interest"),
                    values=c('blue','red'))+
  scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=10),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


####
#ANALYSIS
#####

library(lme4)
data8a<-data8[data8$question=="moral",]
data8a$answer<-factor(data8a$answer)


logit.contexts1<- glm(answer ~ ip, data = data8a, family = "binomial")
summary(logit.contexts1)
logit.contexts2<- glm(answer ~ ip+context, data = data8a, family = "binomial")
summary(logit.contexts2)
logit.contexts3<- glm(answer ~ ip*context, data = data8a, family = "binomial")
summary(logit.contexts3)

# visualizing models 
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(logit.contexts1, title = "model-1"),
    ggstatsplot::ggcoefstats(logit.contexts2, title = "model-2"),
    ggstatsplot::ggcoefstats(logit.contexts3, title = "model-3"),
    nrow = 1
    #package = "RColorBrewer", # package from which color palette is to be taken
    #palette = "rainbow" # choosing a different color palette
  ),
  filename = "contexts_models.png",
  height = 8,
  width = 15,
  units = "in",
  dpi = 300
)

#moral
data8a<-data8[data8$question=="moral",]
#data collapsed across contexts
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data8a,
    main = answer,
    condition = ip
    #bar.proptest = FALSE
  ),
  filename = "contexts1.png"
)

#rule
data8b<-data8[data8$question=="rule",]
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data8b,
    main = answer,
    condition = ip
    #bar.proptest = FALSE
  ),
  filename = "contexts2.png"
)

data8b.sum <- data8b %>% 
  group_by(ip,question) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    sum = sum(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )


#knowledge
data8c<-data8[data8$question=="know",]
data8c$know2<-1
data8c$know2[data8c$answer==0]<-0
data8c.sum <- data8c %>% 
  group_by(ip) %>%
  summarize(
    n = length(know2),
    mean = mean(know2, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )
chisq.test(data8c$know2,data8c$ip,correct=TRUE) #change to correct=FALSE to get the number from ggstats plots
#X-squared = 4.913, df = 1, p-value = 0.02666
#but *fewer* people know about it in the high interest condition, so doesnt seem like it would explain the effect

ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data8c,
    main = know2,
    condition = ip
    #bar.proptest = FALSE
  ),
  filename = "contexts3.png"
)

data8d<-data8[data8$question=="know" | data8$question=="moral",]
data8d<-spread(data8d,question,answer)
logit11a<- glm(moral ~ ip, data = data8d, family = "binomial") 
logit11b<- glm(moral ~ ip + know, data = data8d, family = "binomial") 
logit11c<- glm(moral ~ ip * know, data = data8d, family = "binomial") 
summary(logit11a)
summary(logit11b)
summary(logit11c)

# visualizing models 
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(logit11a, title = "model-1"),
    ggstatsplot::ggcoefstats(logit11b, title = "model-2"),
    ggstatsplot::ggcoefstats(logit11c, title = "model-3"),
    nrow = 1
    #package = "RColorBrewer", # package from which color palette is to be taken
    #palette = "rainbow" # choosing a different color palette
  ),
  filename = "models-know.png",
  height = 8,
  width = 15,
  units = "in",
  dpi = 300
)

#moral with knowlege check
#knowledge flags
data8e<-data8
data8e$flag[data8e$question=="know" & data8e$answer!=0]<-1

#summarize data to flag subjects to exclude
data8e.exclude <- data8e %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
data8e<-join(data8e,data8e.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)
#take out excluded subjects
data8e<-data8e[data8e$exclude==0,]

data8e<-data8e[data8e$question=="moral",]
#data collapsed across contexts
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data8e,
    main = answer,
    condition = ip
    #bar.proptest = FALSE
  ),
  filename = "contexts4.png"
)


####
#reason-endorsement for the fishing case
#####


reasons2.w<-read.csv("reasons_fish.csv")
length(reasons2.w$subjectcode) #n=202
reasons2.w<-head(reasons2.w,-2) #remove last 2 rows

reasons2<-reasons2.w %>%
  gather(question.code,answer,-c(1:3,12:19),na.rm=TRUE) 
reasons2$question.code = factor(reasons2$question.code)
#character vectors to label the questions
question.list<-c(rep(c("outcome","unfair","universalization","harm"),2)) 
context.list<-c(rep("fishing1",4),rep("fishing2",4))
#adds the labels to the long-form data
reasons2<-reasons2 %>%
  mutate(
    question=factor(question.list[question.code]),
    context=context.list[question.code]
  )
reasons2<-reasons2[-c(12)]


#exclusion flags
reasons2$flag<-NA
reasons2$flag[reasons2$check1==1]<-1
reasons2$flag[reasons2$check2==1]<-1
reasons2$flag[reasons2$check3==1]<-1
reasons2$flag[reasons2$check4==1]<-1
reasons2$flag[reasons2$check5==1]<-1
reasons2$flag[reasons2$check6==1]<-1
reasons2$flag[reasons2$check7!="I am paying attention"]<-1

#take out excluded subjects
reasons2<-reasons2[is.na(reasons2$flag),]
reasons2<-reasons2[-c(4:10,15)] #data set is now clean

#summarize data
reasons2.sum <- reasons2 %>% 
  group_by(question,context) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph
reasons2.sum %>%
  ggplot(aes(y=mean, x=context, fill=question)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = .85, xend = 2, yend = .85))+
  xlab("Condition")+
  ylab("P (convincing)")+
  #scale_fill_manual(name = "Condition", 
  #labels = c("High Interest", "Low Interest"),
  #values=c('blue','red'))+
  #scale_x_discrete(labels=c("Catching Fish", "Hunting Birds", "Finding Clams","Foraging Mushrooms","Trapping Rabbits"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


reasonsfish1<-reasons2[reasons2$context=="fishing1",]
reasonsfish1<-reasonsfish1[reasonsfish1$question=="universalization" | reasonsfish1$question=="outcome",]

reasonsfish2<-reasons2[reasons2$context=="fishing1",]
reasonsfish2<-reasonsfish2[reasonsfish2$question=="universalization" | reasonsfish2$question=="harm",]

reasonsfish3<-reasons2[reasons2$context=="fishing1",]
reasonsfish3<-reasonsfish3[reasonsfish3$question=="universalization" | reasonsfish3$question=="unfair",]

reasonsfish4<-reasons2[reasons2$context=="fishing2",]
reasonsfish4<-reasonsfish4[reasonsfish4$question=="universalization" | reasonsfish4$question=="outcome",]

reasonsfish5<-reasons2[reasons2$context=="fishing2",]
reasonsfish5<-reasonsfish5[reasonsfish5$question=="universalization" | reasonsfish5$question=="harm",]

reasonsfish6<-reasons2[reasons2$context=="fishing2",]
reasonsfish6<-reasonsfish6[reasonsfish6$question=="universalization" | reasonsfish6$question=="unfair",]

chisq.test(reasonsfish1$answer,reasonsfish1$question,correct=FALSE) #X-squared = 57.869, df = 1, p-value = 2.802e-14
chisq.test(reasonsfish2$answer,reasonsfish2$question,correct=FALSE) #X-squared = 50.565, df = 1, p-value = 1.153e-12
chisq.test(reasonsfish3$answer,reasonsfish3$question,correct=FALSE) #X-squared = 7.997, df = 1, p-value = 0.004686
chisq.test(reasonsfish4$answer,reasonsfish4$question,correct=FALSE) #X-squared = 56.787, df = 1, p-value = 4.857e-14
chisq.test(reasonsfish5$answer,reasonsfish5$question,correct=FALSE) #X-squared = 54.45, df = 1, p-value = 1.595e-13
chisq.test(reasonsfish6$answer,reasonsfish6$question,correct=FALSE) #X-squared = 16.269, df = 1, p-value = 5.496e-05


#chi-square tests

reasons3<-reasons[reasons$condition=="universalization",]
reasons3<-reasons3[reasons3$question=="universalization" | reasons3$question=="outcome",]

chisq.test(reasons3$answer,reasons3$question,correct=FALSE) 
#X-squared = 259.77, df = 1, p-value < 2.2e-16





########
######
#Study 2b (in the main text)
######
######

data13.w<-read.csv("limitR2_v4.csv")
length(data13.w$subjectcode) #n=433
data13.w<-head(data13.w,-2)
length(data13.w$subjectcode) #n=431, this yields 250 usable subjects as preregistered

data13<-data13.w %>%
  gather(question.code,answer,-c(1:3,20),na.rm=TRUE) 
data13$question.code = factor(data13$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","prediction","moral","rule","outcome"),2)) 
condition.list<-c(rep(c(rep("limit",8),rep("no.limit",8)),2))
#adds the labels to the long-form data
data13<-data13 %>%
  mutate(
    question=factor(question.list[question.code]),
    condition=condition.list[question.code]
  )
data13<-data13[-c(5)]


#exclusion flags
data13$flag<-NA
data13$flag[data13$question=="interest" & data13$answer!=19]<-1
data13$flag[data13$question=="fishermen"]<-1
data13$flag[data13$question=="fishermen" & data13$answer==19]<-NA
data13$flag[data13$question=="fishermen" & data13$answer==20]<-NA
data13$flag[data13$question=="fishermen" & data13$answer==21]<-NA
data13$flag[data13$question=="usage" & data13$answer!=0]<-1
data13$flag[data13$question=="outcome" & data13$answer!=3]<-1
data13$flag[data13$question=="prediction" & data13$answer!=1]<-1


#summarize exclusion data
data13.sum.exclude <- data13 %>% 
  group_by(condition,question) %>%
  summarize(
    n = length(answer),
    exclude = sum(flag, na.rm=T)
  )

#summarize data to flag subjects to exclude
data13.exclude <- data13 %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
data13<-join(data13,data13.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)


#take out excluded subjects
data13<-data13[data13$exclude==0,]
data13<-data13[-c(8:9)] #data set is now clean

#summarize data
data13.sum <- data13 %>% 
  group_by(condition,question) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph 
data13.sum %>%
  filter(question=="moral")%>%
  ggplot(aes(y=mean, x=condition, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = 1.5, y = 1, label = "*", size=10)+
  #geom_segment(aes(x = 1, y = 1, xend = 2, yend = 1))+
  xlab("Condition")+
  ylab("P (morally acceptable)")+
  #scale_x_discrete(labels=c("Threshold", "No Threshold"))+
  scale_fill_manual("Condition",
                    values=c("limit" = "#AAD6B0","no.limit" = "#E0CDE7"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

data13b<-data13[data13$question=="moral",]
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data13b,
    main = answer,
    condition = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "limit2.png",
  height = 4,
  width = 5.5,
  units = "in",
  dpi = 300
)

########
######
#Study 2b - Replication 1 (in the supplement)
#traditionalism as the reason
######
######


data7.w<-read.csv("limit.csv")
data7.w<-head(data7.w,-3) #
length(data7.w$subjectcode) #n=350 as preregistered

data7<-data7.w %>%
  gather(question.code,answer,-c(1:3,18),na.rm=TRUE) 
data7$question.code = factor(data7$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","moral","rule","outcome"),2)) 
condition.list<-c(rep(c(rep("limit",7),rep("no.limit",7)),2))
#adds the labels to the long-form data
data7<-data7 %>%
  mutate(
    question=factor(question.list[question.code]),
    condition=condition.list[question.code]
  )
data7<-data7[-c(5)]


#exclusion flags
data7$flag<-NA
data7$flag[data7$question=="interest" & data7$answer!=19]<-1
data7$flag[data7$question=="fishermen"]<-1
data7$flag[data7$question=="fishermen" & data7$answer==19]<-NA
data7$flag[data7$question=="fishermen" & data7$answer==20]<-NA
data7$flag[data7$question=="fishermen" & data7$answer==21]<-NA
data7$flag[data7$question=="usage" & data7$answer!=0]<-1
data7$flag[data7$question=="outcome" & data7$answer!=3]<-1


#summarize exclusion data
data7.sum.exclude <- data7 %>% 
  group_by(condition,question) %>%
  summarize(
    n = length(answer),
    exclude = sum(flag, na.rm=T)
  )

#summarize data to flag subjects to exclude
data7.exclude <- data7 %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
data7<-join(data7,data7.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)


#take out excluded subjects
data7<-data7[data7$exclude==0,]
data7<-data7[-c(8:9)] #data set is now clean

#summarize data
data7.sum <- data7 %>% 
  group_by(condition,question) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph 
data7.sum %>%
  filter(question=="moral")%>%
  ggplot(aes(y=mean, x=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  geom_text(x = 1.5, y = 1, label = "*", size=10)+
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 1))+
  xlab("Condition")+
  ylab("P (morally acceptable)")+
  scale_x_discrete(labels=c("Threshold", "No Threshold"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

data7b<-data7[data7$question=="moral",]
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data7b,
    main = answer,
    condition = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "limit1.png",
  height = 4,
  width = 5.5,
  units = "in",
  dpi = 300
)


########
######
#Study 2b - Replication 2 (in the supplement)
#wrong as the reason
######
######



data9.w<-read.csv("limit2.csv")
data9.w<-head(data9.w,-2) #
length(data9.w$subjectcode) #n=350 as preregistered

data9<-data9.w %>%
  gather(question.code,answer,-c(1:3,18),na.rm=TRUE) 
data9$question.code = factor(data9$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","moral","rule","outcome"),2)) 
condition.list<-c(rep(c(rep("limit",7),rep("no.limit",7)),2))
#adds the labels to the long-form data
data9<-data9 %>%
  mutate(
    question=factor(question.list[question.code]),
    condition=condition.list[question.code]
  )
data9<-data9[-c(5)]


#exclusion flags
data9$flag<-NA
data9$flag[data9$question=="interest" & data9$answer!=19]<-1
data9$flag[data9$question=="fishermen"]<-1
data9$flag[data9$question=="fishermen" & data9$answer==19]<-NA
data9$flag[data9$question=="fishermen" & data9$answer==20]<-NA
data9$flag[data9$question=="fishermen" & data9$answer==21]<-NA
data9$flag[data9$question=="usage" & data9$answer!=0]<-1
data9$flag[data9$question=="outcome" & data9$answer!=3]<-1


#summarize exclusion data
data9.sum.exclude <- data9 %>% 
  group_by(condition,question) %>%
  summarize(
    n = length(answer),
    exclude = sum(flag, na.rm=T)
  )

#summarize data to flag subjects to exclude
data9.exclude <- data9 %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
data9<-join(data9,data9.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)


#take out excluded subjects
data9<-data9[data9$exclude==0,]
data9<-data9[-c(8:9)] #data set is now clean

#summarize data
data9.sum <- data9 %>% 
  group_by(condition,question) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

#graph
data9.sum %>%
  filter(question=="moral")%>%
  ggplot(aes(y=mean, x=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  #geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  geom_text(x = 1.5, y = 1, label = "***", size=10)+
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 1))+
  xlab("Condition")+
  ylab("P (morally acceptable)")+
  scale_x_discrete(labels=c("Threshold", "No Threshold"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

data9b<-data9[data9$question=="moral",]
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data9b,
    main = answer,
    condition = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "limit2.png",
  height = 4,
  width = 5.5,
  units = "in",
  dpi = 300
)


###
#STUDY 3
#motor oil
####

eu.w<-read.csv("motoroil5.csv")
length(eu.w$subjectcode) #402

#screen out the ppl who failed attention check (the number of boats question)
eu.w<-eu.w[eu.w$d==20 | is.na(eu.w$d),]
eu.w<-eu.w[eu.w$i==20 | is.na(eu.w$i),]
eu.w<-eu.w[eu.w$n==20| is.na(eu.w$n),]
eu.w<-eu.w[eu.w$s==20| is.na(eu.w$s),]
length(eu.w$subjectcode) #386

#remove the cols that contain attn check questions
eu.w<-eu.w[-c(7,12,17,22)]
#remove cols of NAs
eu.w<-eu.w[-c(7,11,15)]

library(tibble)
#these cols will hold the "alleu" calculations
eu.w<-add_column(eu.w, d = NA, .after = "c") 
eu.w<-add_column(eu.w, i = NA, .after = "h")
eu.w<-add_column(eu.w, n = NA, .after = "m")
eu.w<-add_column(eu.w, s = NA, .after = "r")

#high, tour
eu.w$d[eu.w$b==1]<-1
eu.w$d[eu.w$b==2]<-2
eu.w$d[eu.w$b==3 & eu.w$a==2]<-2
eu.w$d[eu.w$b==3 & eu.w$a==1]<-1
eu.w$d[eu.w$b==3 & eu.w$a==3]<-3
#high, fisherman
eu.w$i[eu.w$g==1]<-1
eu.w$i[eu.w$g==2]<-2
eu.w$i[eu.w$g==3 & eu.w$f==2]<-2
eu.w$i[eu.w$g==3 & eu.w$f==1]<-1
eu.w$i[eu.w$g==3 & eu.w$f==3]<-3
#low, tour
eu.w$n[eu.w$l==1]<-1
eu.w$n[eu.w$l==2]<-2
eu.w$n[eu.w$l==3 & eu.w$k==2]<-2
eu.w$n[eu.w$l==3 & eu.w$k==1]<-1
eu.w$n[eu.w$l==3 & eu.w$k==3]<-3
#low, fisherman
eu.w$s[eu.w$q==1]<-1
eu.w$s[eu.w$q==2]<-2
eu.w$s[eu.w$q==3 & eu.w$p==2]<-2
eu.w$s[eu.w$q==3 & eu.w$p==1]<-1
eu.w$s[eu.w$q==3 & eu.w$p==3]<-3

#convert to long form
eu<-eu.w %>%
  gather(question.code,answer,-c(1:3,20),na.rm=TRUE) 
eu$question.code = factor(eu$question.code)
#character vectors to label the questions
question.list<-rep(c("john","fishermen","fmeans","alleu"),4)
context.list<-c(rep(c(rep("tour",4),rep("fisherman",4)),2))
condition.list<-c(rep("high",8),rep("low",8))

#adds the labels to the long-form data
eu<-eu %>%
  mutate(
    question=factor(question.list[question.code]),
    context=context.list[question.code],
    condition=condition.list[question.code]
  )
eu<-eu[-c(5)]
eu$answer[eu$answer==1]<-"better"
eu$answer[eu$answer==2]<-"worse"
eu$answer[eu$answer==3]<-"same"
#convert to two categories rather than 3
eu$answer3[eu$answer=="better"|eu$answer=="same"]<-1
eu$answer3[eu$answer=="worse"]<-0

#summarize data in two categories
eu.sum <- eu %>% 
  group_by(condition,context,question) %>%
  summarize(
    n = length(answer3),
    mean = mean(answer3, na.rm = T),
    se = sqrt(mean*(1-mean)/n))


#barplots that display percentages of people who chose each outcome option

eu1<-eu[eu$context=="tour" & eu$condition=="high",]
eu1.table<-prop.table(table(eu1$answer,eu1$question),margin=2) #margin=2 adds to 100 within a col
#eu1.table<-table(eu1$answer,eu1$question)
barplot(eu1.table, main="John has tour boat; Everyone acts",
        xlab="Outcome Question", col=c("darkblue","red","grey"))
#legend = rownames(eu1.table))

eu2<-eu[eu$context=="tour" & eu$condition=="low",]
eu2.table<-prop.table(table(eu2$answer,eu2$question),margin=2) #margin=2 adds to 100 within a col
barplot(eu2.table, main="John has tour boat; Only John acts",
        xlab="Outcome Question", col=c("darkblue","red","grey"))
#legend = rownames(eu2.table))

eu3<-eu[eu$context=="fisherman" & eu$condition=="high",]
eu3.table<-prop.table(table(eu3$answer,eu3$question),margin=2) #margin=2 adds to 100 within a col
#eu3.table<-table(eu3$answer,eu3$question)
barplot(eu3.table, main="John is a fisherman; Everyone acts",
        xlab="Outcome Question", col=c("darkblue","red","grey"),
        legend = rownames(eu3.table))

eu4<-eu[eu$context=="fisherman" & eu$condition=="low",]
eu4.table<-prop.table(table(eu4$answer,eu4$question),margin=2) #margin=2 adds to 100 within a col
barplot(eu4.table, main="John is a fisherman; Only John acts",
        xlab="Outcome Question", col=c("darkblue","red","grey"),
        legend = rownames(eu4.table))

eu5<-eu[eu$context=="tour" & eu$question=="fishermen",]
eu5$answer2<-eu5$answer
eu5$answer2[eu5$answer=="same" | eu5$answer=="better"]<-"no worse"
eu5.table<-prop.table(table(eu5$answer2,eu5$condition),margin=2) #margin=2 adds to 100 within a col
barplot(eu5.table, main="Tour boat Context; EU Fishermen Q",
        xlab="Condition", col=c("darkblue","grey"),
        legend = rownames(eu5.table))

#phi (effect size) = sqrt(chi-square/#observations)
#number observations = 199
#chi-sq 102.76
#phi = 0.7185972


eu6<-eu[eu$context=="tour" & eu$question=="john",]
eu6$answer2<-eu6$answer
eu6$answer2[eu6$answer=="same" | eu6$answer=="better"]<-"no worse"
eu6$answer2<-factor(eu6$answer2)
eu6$context<-factor(eu6$context)
eu6$answer<-factor(eu6$answer)
eu6$condition<-factor(eu6$condition)
eu6.table<-prop.table(table(eu6$answer2,eu6$condition),margin=2) #margin=2 adds to 100 within a col
barplot(eu6.table, main="Tour boat Context; EU John Q",
        xlab="Condition", col=c("darkblue","grey"),
        legend = rownames(eu6.table))

#phi (effect size) = sqrt(chi-square/#observations)
#number observations = 199
#chi-sq: 3.1054
#phi = 0.1249201


eu7<-eu[eu$context=="fisherman" & eu$question=="fishermen",]
eu7$answer2<-eu7$answer
eu7$answer2[eu7$answer=="same" | eu7$answer=="better"]<-"no worse"
eu7.table<-prop.table(table(eu7$answer2,eu7$condition),margin=2) #margin=2 adds to 100 within a col
barplot(eu7.table, main="Standard Context; EU Fishermen Q",
        xlab="Condition", col=c("darkblue","grey"),
        legend = rownames(eu7.table))

#phi (effect size) = sqrt(chi-square/#observations)
#number observations = 187
#chi-sq = 44.529
#phi = 0.4879785

eu8<-eu[eu$context=="fisherman" & eu$question=="john",]
eu8$answer2<-eu8$answer
eu8$answer2[eu8$answer=="same" | eu8$answer=="better"]<-"no worse"
eu8.table<-prop.table(table(eu8$answer2,eu8$condition),margin=2) #margin=2 adds to 100 within a col
barplot(eu8.table, main="Standard Context; EU John Q",
        xlab="Condition", col=c("darkblue","grey"),
        legend = rownames(eu8.table))

#number observations = 187
#chi-sq = 45.65
#phi = 0.4940826

eu9<-eu[eu$context=="tour" & eu$question=="fmeans",]
eu9$answer2<-eu9$answer
eu9$answer2[eu9$answer=="same" | eu9$answer=="better"]<-"no worse"
eu9$answer2<-factor(eu9$answer2)
eu9$context<-factor(eu9$context)
eu9$answer<-factor(eu9$answer)
eu9$condition<-factor(eu9$condition)
eu9.table<-prop.table(table(eu9$answer2,eu9$condition),margin=2) #margin=2 adds to 100 within a col
barplot(eu9.table, main="Tour boat Context; Fmeans Q",
        xlab="Condition", col=c("darkblue","grey"),
        legend = rownames(eu9.table))

#phi (effect size) = sqrt(chi-square/#observations)
#number observations = 199
#phi = 0.09411049
#X-squared = 1.7625, df = 1, p-value = 0.1843

eu10<-eu[eu$context=="fisherman" & eu$question=="fmeans",]
eu10$answer2<-eu10$answer
eu10$answer2[eu10$answer=="same" | eu10$answer=="better"]<-"no worse"
eu10.table<-prop.table(table(eu10$answer2,eu10$condition),margin=2) #margin=2 adds to 100 within a col
barplot(eu10.table, main="Standard Context; EU Fishermen Q",
        xlab="Condition", col=c("darkblue","grey"),
        legend = rownames(eu10.table))

#phi (effect size) = sqrt(chi-square/#observations)
#number observations = 187
#phi = 0.4201349
#X-squared = 33.008, df = 1, p-value = 9.177e-09

chisq.test(eu5$answer2,eu5$condition) 
chisq.test(eu6$answer2,eu6$condition) 
chisq.test(eu7$answer2,eu7$condition) 
chisq.test(eu8$answer2,eu8$condition) 
chisq.test(eu9$answer2,eu9$condition) 
chisq.test(eu10$answer2,eu10$condition) 


ftable(eu$question,eu$answer,eu$context) #puts all the counts in one table

#convert to numerical answers
eu6$answer3[eu6$answer2=="no worse"]<-1
eu6$answer3[eu6$answer2=="worse"]<-0

#summarize data
eu6.sum <- eu6 %>% 
  group_by(condition) %>%
  summarize(
    n = length(answer3),
    mean = mean(answer3, na.rm = T),
    se = sqrt(mean*(1-mean)/n))


eu.sum %>%
  filter(context=="tour")%>%
  filter(question=="fishermen" | question=="john" | question=="fmeans")%>%
  ggplot(aes(y=mean, x=question, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = .9, y = 1, label = "*", size=6)+
  #geom_segment(aes(x = .65, y = 1, xend = 1.25, yend = 1))+
  #geom_text(x = 2, y = 1.03, label = "n.s.", size=5)+
  #geom_segment(aes(x = 1.75, y = 1, xend = 2.25, yend = 1))+
  #geom_text(x = 3, y = 1.03, label = "n.s.", size=5)+
  # geom_segment(aes(x = 2.75, y = 1, xend = 3.25, yend = 1))+
  xlab("Utility Queried")+
  ylab("P (no worse off)")+
  scale_x_discrete(labels=c("Fishermen", "John", "Frustrated Means"))+
  scale_fill_manual(name = "Condition", 
                    labels = c("Everyone Acts","Only John Acts"),
                    values=c("#FBB4AE","#B3CDE3"))+
  #ggtitle("Motor Oil Exp; Moral Acceptability")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

eu.sum %>%
  filter(context=="fisherman")%>%
  filter(question=="fishermen" | question=="john" | question=="fmeans")%>%
  ggplot(aes(y=mean, x=question, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = .9, y = 1, label = "*", size=6)+
  # geom_segment(aes(x = .65, y = 1, xend = 1.25, yend = 1))+
  #geom_text(x = 2, y = 1, label = "*", size=5)+
  # geom_segment(aes(x = 1.75, y = 1, xend = 2.25, yend = 1))+
  # geom_text(x = 3, y = 1, label = "*", size=5)+
  # geom_segment(aes(x = 2.75, y = 1, xend = 3.25, yend = 1))+
  xlab("Utility Queried")+
  ylab("P (no worse off)")+
  scale_x_discrete(labels=c("Fishermen", "John","Frustrated Means"))+
  scale_fill_manual(name = "Condition", 
                    labels = c("Everyone Acts","Only John Acts"),
                    values=c("#FBB4AE","#B3CDE3"))+
  #ggtitle("Motor Oil Exp; Moral Acceptability")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#other summaries
eu9.sum <- eu9 %>% 
  group_by(condition) %>%
  summarize(
    n = length(answer3),
    mean = mean(answer3, na.rm = T),
    se = sqrt(mean*(1-mean)/n))

eu10.sum <- eu10 %>% 
  group_by(condition) %>%
  summarize(
    n = length(answer3),
    mean = mean(answer3, na.rm = T),
    se = sqrt(mean*(1-mean)/n))



#######
#MORAL JUDGMENT DATA
##########

oil.w<-read.csv("oil_moral.csv")
length(oil.w$subjectcode) #840

#remove blank cols
oil.w<-oil.w[-c(11,16,18,20,22,27,29,31,33,38,40,42)]

#convert to long form
oil<-oil.w %>%
  gather(question.code,answer,-c(1:3,32),na.rm=TRUE) 
oil$question.code = factor(oil$question.code)
#character vectors to label the questions
question.list<-rep(c("know","interest","population","usage","moral","rule","outcome"),4)
context.list<-c(rep(c(rep("tour",7),rep("fisherman",7)),2))
condition.list<-c(rep("high",14),rep("low",14))

#adds the labels to the long-form data
oil<-oil %>%
  mutate(
    question=factor(question.list[question.code]),
    context=context.list[question.code],
    condition=condition.list[question.code]
  )
oil<-oil[-c(5)]

#create exclusion flags
oil$flag<-NA
oil$flag[oil$condition=="high" & oil$question=="interest" & oil$answer!=19]<-1
oil$flag[oil$condition=="low" & oil$question=="interest" & oil$answer!=0]<-1
oil$flag[oil$question=="population" & oil$answer!=20]<-1
oil$flag[oil$question=="usage" & oil$answer!=0]<-1
oil$flag[oil$question=="outcome" & oil$answer!=3]<-1


#summarize data to flag subjects to exclude
oil.exclude <- oil %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
oil<-join(oil,oil.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)
#take out excluded subjects
oil<-oil[oil$exclude==0,]
oil<-oil[-c(9:10)] #data set is now clean


#summarize data
oil.sum <- oil %>% 
  group_by(question,context,condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt(mean*(1-mean)/n))
oil.sum<-oil.sum[oil.sum$question=="moral",]
sum(oil.sum$n) #556

#graph
oil.sum %>%
  ggplot(aes(y=mean, x=context, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = .85, y = .9, label = "*", size=10)+
  # geom_segment(aes(x = .5, y = .85, xend = 1.25, yend = .85))+
  #geom_text(x = 1.85, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1.5, y = .85, xend = 2.25, yend = .85))+
  xlab("Context")+
  ylab("P (morally acceptable)")+
  scale_x_discrete(labels=c("Fisherman", "Tour Boat"))+
  scale_fill_manual(name = "Condition", 
                    labels = c("High Interest","Low Interest"),
                    values=c("#FBB4AE","#B3CDE3"))+
  #ggtitle("Motor Oil Exp; Moral Acceptability")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12),
        legend.title=element_text(size=18), 
        legend.text=element_text(size=18))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



#graph by separate contexts
oil.sum %>%
  filter(context=="tour")%>%
  ggplot(aes(y=mean, x=context, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  geom_text(x = .9, y = .9, label = "*", size=10)+
  geom_segment(aes(x = .65, y = .85, xend = 1.25, yend = .85))+
  #geom_text(x = 1.85, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1.5, y = .85, xend = 2.25, yend = .85))+
  xlab("Context")+
  ylab("P (morally acceptable)")+
  scale_x_discrete(labels=c("Tour Boat"))+
  scale_fill_manual(name = "Condition", 
                    labels = c("High Interest","Low Interest"),
                    values=c("blue","red"))+
  #ggtitle("Motor Oil Exp; Moral Acceptability")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

oil.sum %>%
  filter(context=="fisherman")%>%
  ggplot(aes(y=mean, x=context, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  geom_text(x = .9, y = .9, label = "*", size=10)+
  geom_segment(aes(x = .65, y = .85, xend = 1.25, yend = .85))+
  #geom_text(x = 1.85, y = .9, label = "*", size=10)+
  #geom_segment(aes(x = 1.5, y = .85, xend = 2.25, yend = .85))+
  xlab("Context")+
  ylab("P (morally acceptable)")+
  scale_x_discrete(labels=c("Fisherman"))+
  scale_fill_manual(name = "Condition", 
                    labels = c("High Interest","Low Interest"),
                    values=c("gray78","gray28"))+
  #ggtitle("Motor Oil Exp; Moral Acceptability")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#####
#regressions
#####

oil.m<-oil[oil$question=="moral",]
glm1 <- glm(answer~ context*condition, family = "binomial",data=oil.m)
glm2 <- glm(answer~ context, family = "binomial",data=oil.m)
#nova(glm1,glm2)
summary(glm1)
summary(glm2)



##########
#########
#STUDY 4a
#########
##########

one.w<-read.csv("4-7b.csv")
one.w<-one.w[complete.cases(one.w),]
two.w<-read.csv("10-13b.csv")
two.w<-two.w[complete.cases(two.w),]
one.w$condition<-"4-7"
two.w$condition<-"10-13"
moral.w<-rbind(one.w,two.w)
length(moral.w$Abaser) #n= 700 subjects in the exp

#screen out those who failed the "know" question
# moral.w<-moral.w[moral.w$Abaser==0 |moral.w$Abaser==1,]
# moral.w<-moral.w[moral.w$Abdest==0 |moral.w$Abdest==1,]
# moral.w<-moral.w[moral.w$Acacin==0 |moral.w$Acacin==1,]
# moral.w<-moral.w[moral.w$Access==0 |moral.w$Access==1,]
# moral.w<-moral.w[moral.w$Achean==0 |moral.w$Achean==1,]
# moral.w<-moral.w[moral.w$g==0 |moral.w$g==1,] #316

#screen out the ppl who failed the number of fishermen question
moral.w<-moral.w[moral.w$Abater==20 | moral.w$Abater==21 | moral.w$Abater==19,]
moral.w<-moral.w[moral.w$Abject==20 |moral.w$Abject==21 |moral.w$Abject==19,]
moral.w<-moral.w[moral.w$Acanth==20 | moral.w$Acanth==21| moral.w$Acanth==19,]
moral.w<-moral.w[moral.w$Accrue==20 | moral.w$Accrue==21 | moral.w$Accrue==19,]
moral.w<-moral.w[moral.w$b==20 | moral.w$b==21 | moral.w$b==19,]
moral.w<-moral.w[moral.w$i==20 | moral.w$b==21 | moral.w$b==19,] 
length(moral.w$Abaser) #615

#screen on interest
moral.w<-moral.w[moral.w$Abassi==2,]
moral.w<-moral.w[moral.w$Abider==7,]
moral.w<-moral.w[moral.w$Acajou==8,]
moral.w<-moral.w[moral.w$Accost==13,]
moral.w<-moral.w[moral.w$Achene==19,]
moral.w<-moral.w[moral.w$h==0,] 
length(moral.w$Abaser) #437

#screen on usage
moral.w<-moral.w[moral.w$Abatis==0,]
moral.w<-moral.w[moral.w$Ablaut==0,]
moral.w<-moral.w[moral.w$Acarus==0,]
moral.w<-moral.w[moral.w$Accuse==0,]
moral.w<-moral.w[moral.w$c==0,]
moral.w<-moral.w[moral.w$j==0,]
length(moral.w$Abaser) #409

#this screens out subjects who ever gave an outcome answer other than 3 (no effect of John fishing)
moral.w<-moral.w[moral.w$Abbess==3,]
moral.w<-moral.w[moral.w$Acacia==3,]
moral.w<-moral.w[moral.w$Accent==3,]
moral.w<-moral.w[moral.w$Achate==3,]
moral.w<-moral.w[moral.w$f==3,]
moral.w<-moral.w[moral.w$m==3,] 
length(moral.w$Abaser) #350


#one data frame has subject answers
#the other has the number that was shown for each question
moral.ques.w<-moral.w[c(1:46,55)]
moral.rand.w<-moral.w[c(1:3,47,49,51,53)]

#for first question only analysis
# moral.ques.w<-moral.w[c(1:46,61)]
# moral.rand.w<-moral.w[c(1:3,47,49,51,53)]
# moral.order.w<-moral.w[c(1:3,55:60)]

#data frame with answers to questions
moral.ques<-moral.ques.w %>%
  gather(question.code,answer,-c(1:3,46:47),na.rm=TRUE) 
moral.ques$question.code = factor(moral.ques$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","moral","rule","outcome"),6)) 
number.list<-c(rep(2,7),rep(7,7),rep(8,7),rep(13,7),rep(19,7),rep(0,7))
#adds the labels to the long-form data
moral.ques<-moral.ques %>%
  mutate(
    question=factor(question.list[question.code]),
    number=number.list[question.code]
  )
moral.ques<-moral.ques[c(1:5,7:9)]
moral<-moral.ques

#summarize data
moral.sum <- moral %>% 
  group_by(question,number,condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )


#graph: moral
moral.sum %>%
  filter(question=="moral")%>%
  #filter(condition=="4-7")%>%
  ggplot(aes(y=mean, x=number, color=condition)) + 
  geom_point()+
  geom_line() +
  #geom_line(aes(y = 0.5, color="red"))+
  coord_cartesian(ylim = c(.4, 1))+
  scale_x_continuous(breaks=c(0,2,7,8,13,19))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(0.05))+
  theme(legend.position="right")+
  xlab("Number of interested parties")+
  ylab("P (morally acceptable)")+
  theme(legend.position="right",
        legend.text = element_text(size = "18"),
        legend.title= element_text(size="20"))+
  scale_color_manual(name="Threshold",
                     labels = c("10,13",
                                "4,7"),
                     values = c("10-13"="darkorchid", 
                                "4-7"="forestgreen"))+
  #ggtitle("Moral Acceptability Curve")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#####
#preregistered analysis
#######

####
#Central analysis
####

#our analysis only works with centered predictors
#Condition should be coded not 0/1 (or left to R’s default coding scheme), 
#but rather hand-coded 0.5 / -0.5.
# And ip should be recoded ip_new = ip_original - mean(ip_original)
# (i.e., mean-centered).
moral.m<-moral[moral$question=="moral",]
moral.m$condition[moral.m$condition=="4-7"]<- 0.5
moral.m$condition[moral.m$condition=="10-13"]<- -0.5
moral.m$condition<-as.numeric(moral.m$condition)
moral.m$number_centered<-moral.m$number - mean(moral.m$number)

#logistic mixed effects model with subject as random variable
glm1 <- glmer(answer~ number_centered + condition + condition*number_centered + (1|subjectcode), family = "binomial",data=moral.m) 
glm2 <- glmer(answer~ number_centered+ (1|subjectcode), family = "binomial",data=moral.m)
glm3 <- glmer(answer~  (1|subjectcode), family = "binomial",data=moral.m)
glm4 <- glmer(answer~ condition + (1|subjectcode), family = "binomial",data=moral.m) 
anova(glm1,glm2)
anova(glm1,glm3)
anova(glm2,glm3)
summary(glm1)
summary(glm2)
summary(glm3)
summary(glm4)


####
#knowledge
#####


#is there a negative slope to the data, linear regression, number predicting know
moral.k<-moral[moral$question=="know",]
lm1<- lm(answer ~ number, data = moral.k)
summary(lm1) #number is not significant


#is there still an effect of number on moral judgment, once allowing knowledge into the regression?
moral.m<-moral[moral$question=="moral",]
moral.m<-moral.m[c(3,5,6,8)]
moral.k<-moral.k[c(3,5,6,8)]
names(moral.m)[names(moral.m) == "answer"] <- "moral"
names(moral.k)[names(moral.k) == "answer"] <- "know"
moral.mk<-merge(moral.m,moral.k)

logit1<- glm(moral ~ number*know, data = moral.mk, family="binomial")
summary(logit1) #number is significant and knowledge is not nor the interaction


####
#rule
#####

moral.r<-moral[moral$question=="rule",]
logit1<- glm(answer ~ number, data = moral.r, family="binomial")
summary(logit1) #number is marginally significant predictor

#is there still an effect of number on moral judgment, once allowing rule into the regression?
moral.m<-moral[moral$question=="moral",]
moral.m<-moral.m[c(3,5,6,8)]
moral.r<-moral.r[c(3,5,6,8)]
names(moral.m)[names(moral.m) == "answer"] <- "moral"
names(moral.r)[names(moral.r) == "answer"] <- "rule"
moral.mr<-merge(moral.m,moral.r)

logit1<- glm(moral ~ number * rule, data = moral.mr, family="binomial")
summary(logit1) #number is significant and rule is not


#########
#INDIVIDUAL DIFFERENCES
###########

moral.w$universalizer<-NA
moral.w$permissible<-NA
moral.w$impermissible<-NA
moral.w$category<-NA #empty
moral.w$v<-NA #havent coded this yet
moral.w$other<-NA
moral.w$universalizer<-0
moral.w$permissible<-0
moral.w$impermissible<-0
moral.w$category<-0
moral.w$v<-0
moral.w$other<-0
length(moral.w$subjectcode) #350

#break subjects into categories
moral.w$universalizer[moral.w$d==0 & moral.w$k==1]<-1 #n=100 people fit this category
moral.w$permissible[moral.w$Abator==1 &
                      moral.w$Abraum==1 &
                      moral.w$Acater==1 &
                      moral.w$Acetal==1 &
                      moral.w$d==1 &
                      moral.w$k==1]<-1 #229
moral.w$impermissible[moral.w$Abator==0 &
                        moral.w$Abraum==0 &
                        moral.w$Acater==0 &
                        moral.w$Acetal==0 &
                        moral.w$d==0 &
                        moral.w$k==0]<-1  #98
moral.w$other[moral.w$universalizer==0 & moral.w$permissible==0 & moral.w$impermissible==0]<-1 #53 subjects are unaccounted for using this coding scheme
moral.w$category[moral.w$universalizer==1]<-"universalizer"
moral.w$category[moral.w$permissible==1]<-"permissible"
moral.w$category[moral.w$impermissible==1]<-"impermissible"
moral.w$category[moral.w$other==1]<-"other"

univ.w<-moral.w[moral.w$universalizer==1,] 
perm.w<-moral.w[moral.w$permissible==1,] 
imp.w<-moral.w[moral.w$impermissible==1,] 
other.w<-moral.w[moral.w$other==1,]
uo.w<-moral.w[moral.w$universalizer==1 | moral.w$other==1,] 
length(univ.w$subjectcode) #76 subjects 
length(perm.w$subjectcode) #n=192
length(imp.w$subjectcode) #n=63
length(other.w$subjectcode) #n=19
length(uo.w$subjectcode) #n=95

###format the data for graphing
#one data frame has subject answers
#the other has the number that was shown for each question
moral.ques.w<-uo.w[c(1:46,55)] #universalizers and other

#data frame with answers to questions
moral.ques<-moral.ques.w %>%
  gather(question.code,answer,-c(1:3,46:47),na.rm=TRUE) 
moral.ques$question.code = factor(moral.ques$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","moral","rule","outcome"),6)) 
number.list<-c(rep(2,7),rep(7,7),rep(8,7),rep(13,7),rep(19,7),rep(0,7))
#adds the labels to the long-form data
moral.ques<-moral.ques %>%
  mutate(
    question=factor(question.list[question.code]),
    number=number.list[question.code]
  )
moral.ques<-moral.ques[c(1:5,7:9)]
moral<-moral.ques
moral<-moral[moral$question=="moral",]

#summarize data
moral.sum <- moral %>% 
  group_by(question,number,condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )


#graph: 
moral.sum %>%
  filter(question=="moral")%>%
  ggplot(aes(y=mean, x=number, color=condition)) + 
  geom_point()+
  geom_line() +
  #geom_line(aes(y = 0.5, color="red"))+
  coord_cartesian(ylim = c(0, 1))+
  scale_x_continuous(breaks=c(0,2,7,8,13,19))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(0.05))+
  theme(legend.position="right")+
  xlab("Number of interested parties")+
  ylab("P (morally acceptable)")+
  theme(legend.position="right",
        legend.text = element_text(size = "18"),
        legend.title= element_text(size="20"))+
  scale_color_manual(name="Threshold",
                     labels = c("10,13",
                                "4,7"),
                     values = c("10-13"="darkorchid", 
                                "4-7"="forestgreen"))+
  #ggtitle("Moral Acceptability Curve")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#just uniform judgments
moral.sum2<-moral.sum
moral.sum2$mean[moral.sum2$condition=="10-13"]<-1
moral.sum2$mean[moral.sum2$condition=="4-7"]<-.99

#graph: 
moral.sum2 %>%
  filter(question=="moral")%>%
  ggplot(aes(y=mean, x=number, color=condition)) + 
  geom_point()+
  geom_line() +
  #geom_line(aes(y = 0.5, color="red"))+
  coord_cartesian(ylim = c(0, 1))+
  scale_x_continuous(breaks=c(0,2,7,8,13,19))+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(0.05))+
  #theme(legend.position="right")+
  xlab("Number of interested parties")+
  ylab("P (morally acceptable)")+
  theme(legend.position="right",
        legend.text = element_text(size = "18"),
        legend.title= element_text(size="20"))+
  scale_color_manual(name="Threshold",
                     labels = c("10,13",
                                "4,7"),
                     values = c("10-13"="darkorchid", 
                                "4-7"="forestgreen"))+
  #ggtitle("Moral Acceptability Curve")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



###########
########
#STUDY 4b
########
########

cas.w<-read.csv("CA_small.csv") 
length(cas.w$subjectcode) #n= 300 subjects in the exp

#convert to long form
cas<-cas.w %>%
  gather(question.code,answer,-c(1:3,46),na.rm=TRUE) 
cas$question.code = factor(cas$question.code)
#character vectors to label the questions
ca.list<-rep(c(rep("alleu",7),rep("johneu",7),rep("means",7)),2)
number.list<-c(rep(c(1,3,8,9,14,20,"check"),6))
threshold.list<-c(rep("4-7",21),rep("10-13",21))
#adds the labels to the long-form data
cas<-cas %>%
  mutate(
    ca.type=factor(ca.list[question.code]),
    number=number.list[question.code],
    threshold=threshold.list[question.code]
  )
cas<-cas[-c(5)]


#create exclusion flags
cas$flag<-NA
cas$flag[cas$number=="check" & cas$answer!=20]<-1


#summarize data to flag subjects to exclude
cas.exclude <- cas %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
cas<-join(cas,cas.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)
#take out excluded subjects
cas<-cas[cas$exclude==0,]
cas<-cas[-c(9:10)] #data set is now clean
#remove check questions
cas<-cas[cas$number!="check",]
cas$number<-as.numeric(cas$number)

#summarize data
cas.sum <- cas %>% 
  group_by(ca.type,number,threshold) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    sd = sd(answer, na.rm = T),
    se = sd/sqrt(n)
  )
#282 total subjects passed the control questions
#96 in the alleu condition

#graph the three different CA curves
cas.sum %>%
  filter(ca.type=="alleu")%>%
  ggplot(aes(y=mean, x=number, color=threshold)) + 
  geom_point()+
  geom_line() +
  coord_cartesian(ylim = c(-50, 50))+
  scale_x_continuous(breaks=c(1,3,8,9,14,20))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(0.05))+
  theme(legend.position="right")+
  xlab("Number of people acting")+
  ylab("Change in Utility for the Vacationers")+
  ggtitle("Collective Action Curve \n(Everyone's EU)")+
  theme(legend.position="right",
        legend.text = element_text(size = "12"),
        legend.title= element_text(size="12"))+
  scale_color_manual(name="Threshold",
                     labels = c("10,13",
                                "4,7"),
                     values = c("10-13"="darkorchid", 
                                "4-7"="forestgreen"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12),
        plot.title = element_text(face="bold", size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

cas.sum %>%
  filter(ca.type=="johneu")%>%
  ggplot(aes(y=mean, x=number, color=threshold)) + 
  geom_point()+
  geom_line() +
  coord_cartesian(ylim = c(-50, 50))+
  scale_x_continuous(breaks=c(1,3,8,9,14,20))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(0.05))+
  theme(legend.position="right")+
  xlab("Number of people acting")+
  ylab("Change in Utility for John")+
  ggtitle("Collective Action Curve \n(John's EU)")+
  theme(legend.position="right",
        legend.text = element_text(size = "12"),
        legend.title= element_text(size="12"))+
  scale_color_manual(name="Threshold",
                     labels = c("10,13",
                                "4,7"),
                     values = c("10-13"="blue", 
                                "4-7"="red"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12),
        plot.title = element_text(face="bold", size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

cas.sum %>%
  filter(ca.type=="means")%>%
  ggplot(aes(y=mean, x=number, color=threshold)) + 
  geom_point()+
  geom_line() +
  coord_cartesian(ylim = c(-50, 50))+
  scale_x_continuous(breaks=c(1,3,8,9,14,20))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(0.05))+
  theme(legend.position="right")+
  xlab("Number of people acting")+
  ylab("Likelihood of John Achieving Goal")+
  ggtitle("Collective Action Curve \n(Frustrated Means)")+
  theme(legend.position="right",
        legend.text = element_text(size = "12"),
        legend.title= element_text(size="12"))+
  scale_color_manual(name="Threshold",
                     labels = c("10,13",
                                "4,7"),
                     values = c("10-13"="blue", 
                                "4-7"="red"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12),
        plot.title = element_text(face="bold", size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


############
#OPTIMIZATION USING LIKELIHOOD
############

###
#get the moral judgment data from study 2
###

one.w<-read.csv("4-7b.csv")
one.w<-one.w[complete.cases(one.w),]
two.w<-read.csv("10-13b.csv")
two.w<-two.w[complete.cases(two.w),]
one.w$condition<-"4-7"
two.w$condition<-"10-13"
moral.w<-rbind(one.w,two.w)
length(moral.w$Abaser) #n= 700 subjects in the exp


#screen out the ppl who failed the number of fishermen question
moral.w<-moral.w[moral.w$Abater==20 | moral.w$Abater==21 | moral.w$Abater==19,]
moral.w<-moral.w[moral.w$Abject==20 |moral.w$Abject==21 |moral.w$Abject==19,]
moral.w<-moral.w[moral.w$Acanth==20 | moral.w$Acanth==21| moral.w$Acanth==19,]
moral.w<-moral.w[moral.w$Accrue==20 | moral.w$Accrue==21 | moral.w$Accrue==19,]
moral.w<-moral.w[moral.w$b==20 | moral.w$b==21 | moral.w$b==19,]
moral.w<-moral.w[moral.w$i==20 | moral.w$b==21 | moral.w$b==19,] 
length(moral.w$Abaser) #615

#screen on interest
moral.w<-moral.w[moral.w$Abassi==2,]
moral.w<-moral.w[moral.w$Abider==7,]
moral.w<-moral.w[moral.w$Acajou==8,]
moral.w<-moral.w[moral.w$Accost==13,]
moral.w<-moral.w[moral.w$Achene==19,]
moral.w<-moral.w[moral.w$h==0,] 
length(moral.w$Abaser) #437

#screen on usage
moral.w<-moral.w[moral.w$Abatis==0,]
moral.w<-moral.w[moral.w$Ablaut==0,]
moral.w<-moral.w[moral.w$Acarus==0,]
moral.w<-moral.w[moral.w$Accuse==0,]
moral.w<-moral.w[moral.w$c==0,]
moral.w<-moral.w[moral.w$j==0,]
length(moral.w$Abaser) #409

#this screens out subjects who ever gave an outcome answer other than 3 (no effect of John fishing)
moral.w<-moral.w[moral.w$Abbess==3,]
moral.w<-moral.w[moral.w$Acacia==3,]
moral.w<-moral.w[moral.w$Accent==3,]
moral.w<-moral.w[moral.w$Achate==3,]
moral.w<-moral.w[moral.w$f==3,]
moral.w<-moral.w[moral.w$m==3,] 
length(moral.w$Abaser) #350


#one data frame has subject answers
#the other has the number that was shown for each question
moral.ques.w<-moral.w[c(1:46,55)]
moral.rand.w<-moral.w[c(1:3,47,49,51,53)]

#data frame with answers to questions
moral.ques<-moral.ques.w %>%
  gather(question.code,answer,-c(1:3,46:47),na.rm=TRUE) 
moral.ques$question.code = factor(moral.ques$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","moral","rule","outcome"),6)) 
number.list<-c(rep(2,7),rep(7,7),rep(8,7),rep(13,7),rep(19,7),rep(0,7))
#adds the labels to the long-form data
moral.ques<-moral.ques %>%
  mutate(
    question=factor(question.list[question.code]),
    number=number.list[question.code]
  )
moral.ques<-moral.ques[c(1:5,7:9)]
moral<-moral.ques

#summarize data
moral.sum <- moral %>% 
  group_by(question,number,condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

####back to the ca curves

#create data frames that contain the mean response at each ip for 4,7 and 10,13 thresholds
cas.sum2<-cas.sum[c(1,2,3,5)]
alleu<-cas.sum2[cas.sum2$ca.type=="alleu",]
alleu<-alleu[-c(1)]
names(alleu)<-c("number","threshold","alleu")
johneu<-cas.sum2[cas.sum2$ca.type=="johneu",]
johneu<-johneu[-c(1)]
names(johneu)<-c("number","threshold","johneu")
fmeans<-cas.sum2[cas.sum2$ca.type=="means",]
fmeans<-fmeans[-c(1)]
names(fmeans)<-c("number","threshold","fmeans")


lld<-moral[moral$question=="moral",] 
lld<-lld[c(5,6,8)]
names(lld)<-c("threshold","data","number")
lld$number[lld$number==0]<-1
lld$number[lld$number==2]<-3
lld$number[lld$number==7]<-8
lld$number[lld$number==8]<-9
lld$number[lld$number==13]<-14
lld$number[lld$number==19]<-20
lld.all<-lld #lld.all is dataframe holding indiv subject respones to each moral judgment question


#Likelihood optimization
###
#THRESHOLD 4-7
#####


#THRESHOLD: 4-7
#Everyone's EU
lld<-lld.all[lld.all$threshold=="4-7",]
lld$alleu<-NA

optim7<-function(beta){
  lld$alleu[lld$number ==1 & lld$data==0]<-1-(.2/(1+exp(-beta*alleu[2,3]))+.6)
  lld$alleu[lld$number ==1 & lld$data==1]<-.2/(1+exp(-beta*alleu[2,3]))+.6
  lld$alleu[lld$number ==3 & lld$data==0]<-1-(.2/(1+exp(-beta*alleu[4,3]))+.6)
  lld$alleu[lld$number ==3 & lld$data==1]<-.2/(1+exp(-beta*alleu[4,3]))+.6
  lld$alleu[lld$number ==8 & lld$data==0]<-1-(.2/(1+exp(-beta*alleu[6,3]))+.6)
  lld$alleu[lld$number ==8 & lld$data==1]<-.2/(1+exp(-beta*alleu[6,3]))+.6
  lld$alleu[lld$number ==9 & lld$data==0]<-1-(.2/(1+exp(-beta*alleu[8,3]))+.6)
  lld$alleu[lld$number ==9 & lld$data==1]<-.2/(1+exp(-beta*alleu[8,3]))+.6
  lld$alleu[lld$number ==14 & lld$data==0]<-1-(.2/(1+exp(-beta*alleu[10,3]))+.6)
  lld$alleu[lld$number ==14 & lld$data==1]<-.2/(1+exp(-beta*alleu[10,3]))+.6
  lld$alleu[lld$number ==20 & lld$data==0]<-1-(.2/(1+exp(-beta*alleu[12,3]))+.6)
  lld$alleu[lld$number ==20 & lld$data==1]<-.2/(1+exp(-beta*alleu[12,3]))+.6
  lld$alleu<-as.numeric(lld$alleu)
  lld$nll<--log(lld$alleu)
  nll=sum(lld$nll)
  return(nll)
}
optim(c(.4), optim7, method="Brent",lower = -10, upper =10) 
#beta=0.2415495
#sum of nll= 653.9203
#AIC = 2+2nll =  1309.841

#THRESHOLD: 4-7
#John's EU
lld<-lld.all[lld.all$threshold=="4-7",]
lld$johneu<-NA

optim7<-function(beta){
  ##try separating each computational step into a different col for better visibility
  lld$johneu[lld$number ==1 & lld$data==0]<-1-(.2/(1+exp(-beta*johneu[2,3]))+.6)
  lld$johneu[lld$number ==1 & lld$data==1]<-.2/(1+exp(-beta*johneu[2,3]))+.6
  lld$johneu[lld$number ==3 & lld$data==0]<-1-(.2/(1+exp(-beta*johneu[4,3]))+.6)
  lld$johneu[lld$number ==3 & lld$data==1]<-.2/(1+exp(-beta*johneu[4,3]))+.6
  lld$johneu[lld$number ==8 & lld$data==0]<-1-(.2/(1+exp(-beta*johneu[6,3]))+.6)
  lld$johneu[lld$number ==8 & lld$data==1]<-.2/(1+exp(-beta*johneu[6,3]))+.6
  lld$johneu[lld$number ==9 & lld$data==0]<-1-(.2/(1+exp(-beta*johneu[8,3]))+.6)
  lld$johneu[lld$number ==9 & lld$data==1]<-.2/(1+exp(-beta*johneu[8,3]))+.6
  lld$johneu[lld$number ==14 & lld$data==0]<-1-(.2/(1+exp(-beta*johneu[10,3]))+.6)
  lld$johneu[lld$number ==14 & lld$data==1]<-.2/(1+exp(-beta*johneu[10,3]))+.6
  lld$johneu[lld$number ==20 & lld$data==0]<-1-(.2/(1+exp(-beta*johneu[12,3]))+.6)
  lld$johneu[lld$number ==20 & lld$data==1]<-.2/(1+exp(-beta*johneu[12,3]))+.6
  lld$johneu<-as.numeric(lld$johneu)
  lld$nll<--log(lld$johneu)
  nll=sum(lld$nll)
  return(nll)
}
optim(c(2), optim7, method="Brent",lower = -10, upper =10) 
#beta=2.36068
#sum of nll= 655.0676
#AIC = 2+2nll = 1312.135

#THRESHOLD: 4-7
#Frustrated Means
lld<-lld.all[lld.all$threshold=="4-7",]
lld$fmeans<-NA

optim7<-function(beta){
  ##try separating each computational step into a different col for better visibility
  lld$fmeans[lld$number ==1 & lld$data==0]<-1-(.2/(1+exp(-beta*fmeans[2,3]))+.6)
  lld$fmeans[lld$number ==1 & lld$data==1]<-.2/(1+exp(-beta*fmeans[2,3]))+.6
  lld$fmeans[lld$number ==3 & lld$data==0]<-1-(.2/(1+exp(-beta*fmeans[4,3]))+.6)
  lld$fmeans[lld$number ==3 & lld$data==1]<-.2/(1+exp(-beta*fmeans[4,3]))+.6
  lld$fmeans[lld$number ==8 & lld$data==0]<-1-(.2/(1+exp(-beta*fmeans[6,3]))+.6)
  lld$fmeans[lld$number ==8 & lld$data==1]<-.2/(1+exp(-beta*fmeans[6,3]))+.6
  lld$fmeans[lld$number ==9 & lld$data==0]<-1-(.2/(1+exp(-beta*fmeans[8,3]))+.6)
  lld$fmeans[lld$number ==9 & lld$data==1]<-.2/(1+exp(-beta*fmeans[8,3]))+.6
  lld$fmeans[lld$number ==14 & lld$data==0]<-1-(.2/(1+exp(-beta*fmeans[10,3]))+.6)
  lld$fmeans[lld$number ==14 & lld$data==1]<-.2/(1+exp(-beta*fmeans[10,3]))+.6
  lld$fmeans[lld$number ==20 & lld$data==0]<-1-(.2/(1+exp(-beta*fmeans[12,3]))+.6)
  lld$fmeans[lld$number ==20 & lld$data==1]<-.2/(1+exp(-beta*fmeans[12,3]))+.6
  lld$fmeans<-as.numeric(lld$fmeans)
  lld$nll<--log(lld$fmeans)
  nll=sum(lld$nll)
  return(nll)
}
optim(c(.05), optim7, method="Brent",lower = -10, upper =10) 
#beta=0.04734598
#sum of nll= 658.2062
#AIC = 2+2nll = 1318.412

#####
#THRESHOLD 10,13
#########

#THRESHOLD: 10-13
#Everyone's EU
lld<-lld.all[lld.all$threshold=="10-13",]
lld$alleu<-NA

optim7<-function(beta){
  lld$alleu[lld$number ==1 & lld$data==0]<-1-(.22/(1+exp(-beta*alleu[1,3]))+.58)
  lld$alleu[lld$number ==1 & lld$data==1]<-.22/(1+exp(-beta*alleu[1,3]))+.58
  lld$alleu[lld$number ==3 & lld$data==0]<-1-(.22/(1+exp(-beta*alleu[3,3]))+.58)
  lld$alleu[lld$number ==3 & lld$data==1]<-.22/(1+exp(-beta*alleu[3,3]))+.58
  lld$alleu[lld$number ==8 & lld$data==0]<-1-(.22/(1+exp(-beta*alleu[5,3]))+.58)
  lld$alleu[lld$number ==8 & lld$data==1]<-.22/(1+exp(-beta*alleu[5,3]))+.58
  lld$alleu[lld$number ==9 & lld$data==0]<-1-(.22/(1+exp(-beta*alleu[7,3]))+.58)
  lld$alleu[lld$number ==9 & lld$data==1]<-.22/(1+exp(-beta*alleu[7,3]))+.58
  lld$alleu[lld$number ==14 & lld$data==0]<-1-(.22/(1+exp(-beta*alleu[9,3]))+.58)
  lld$alleu[lld$number ==14 & lld$data==1]<-.22/(1+exp(-beta*alleu[9,3]))+.58
  lld$alleu[lld$number ==20 & lld$data==0]<-1-(.22/(1+exp(-beta*alleu[11,3]))+.58)
  lld$alleu[lld$number ==20 & lld$data==1]<-.22/(1+exp(-beta*alleu[11,3]))+.58
  lld$alleu<-as.numeric(lld$alleu)
  lld$nll<--log(lld$alleu)
  nll=sum(lld$nll)
  return(nll) 
}

optim(c(.4), optim7, method="Brent",lower = -10, upper =10) 
#beta= 0.3191474
#sum of nll= 604.2032
#AIC = 2+2nll =  1210.406


#THRESHOLD: 10-13
#John's EU
lld<-lld.all[lld.all$threshold=="10-13",]
lld$johneu<-NA

optim7<-function(beta){
  lld$johneu[lld$number ==1 & lld$data==0]<-1-(.22/(1+exp(-beta*johneu[1,3]))+.58)
  lld$johneu[lld$number ==1 & lld$data==1]<-.22/(1+exp(-beta*johneu[1,3]))+.58
  lld$johneu[lld$number ==3 & lld$data==0]<-1-(.22/(1+exp(-beta*johneu[3,3]))+.58)
  lld$johneu[lld$number ==3 & lld$data==1]<-.22/(1+exp(-beta*johneu[3,3]))+.58
  lld$johneu[lld$number ==8 & lld$data==0]<-1-(.22/(1+exp(-beta*johneu[5,3]))+.58)
  lld$johneu[lld$number ==8 & lld$data==1]<-.22/(1+exp(-beta*johneu[5,3]))+.58
  lld$johneu[lld$number ==9 & lld$data==0]<-1-(.22/(1+exp(-beta*johneu[7,3]))+.58)
  lld$johneu[lld$number ==9 & lld$data==1]<-.22/(1+exp(-beta*johneu[7,3]))+.58
  lld$johneu[lld$number ==14 & lld$data==0]<-1-(.22/(1+exp(-beta*johneu[9,3]))+.58)
  lld$johneu[lld$number ==14 & lld$data==1]<-.22/(1+exp(-beta*johneu[9,3]))+.58
  lld$johneu[lld$number ==20 & lld$data==0]<-1-(.22/(1+exp(-beta*johneu[11,3]))+.58)
  lld$johneu[lld$number ==20 & lld$data==1]<-.22/(1+exp(-beta*johneu[11,3]))+.58
  lld$johneu<-as.numeric(lld$johneu)
  lld$nll<--log(lld$johneu)
  nll=sum(lld$nll)
  return(nll)
}
optim(c(.1), optim7, method="Brent",lower = -10, upper =10) 
#beta= 2.36068
#sum of nll= 606.0956
#AIC = 2+2nll = 1214.191

#THRESHOLD: 10-13
#Frustrated Means
lld<-lld.all[lld.all$threshold=="10-13",]
lld$fmeans<-NA

optim7<-function(beta){
  lld$fmeans[lld$number ==1 & lld$data==0]<-1-(.22/(1+exp(-beta*fmeans[1,3]))+.58)
  lld$fmeans[lld$number ==1 & lld$data==1]<-.22/(1+exp(-beta*fmeans[1,3]))+.58
  lld$fmeans[lld$number ==3 & lld$data==0]<-1-(.22/(1+exp(-beta*fmeans[3,3]))+.58)
  lld$fmeans[lld$number ==3 & lld$data==1]<-.22/(1+exp(-beta*fmeans[3,3]))+.58
  lld$fmeans[lld$number ==8 & lld$data==0]<-1-(.22/(1+exp(-beta*fmeans[5,3]))+.58)
  lld$fmeans[lld$number ==8 & lld$data==1]<-.22/(1+exp(-beta*fmeans[5,3]))+.58
  lld$fmeans[lld$number ==9 & lld$data==0]<-1-(.22/(1+exp(-beta*fmeans[7,3]))+.58)
  lld$fmeans[lld$number ==9 & lld$data==1]<-.22/(1+exp(-beta*fmeans[7,3]))+.58
  lld$fmeans[lld$number ==14 & lld$data==0]<-1-(.22/(1+exp(-beta*fmeans[9,3]))+.58)
  lld$fmeans[lld$number ==14 & lld$data==1]<-.22/(1+exp(-beta*fmeans[9,3]))+.58
  lld$fmeans[lld$number ==20 & lld$data==0]<-1-(.22/(1+exp(-beta*fmeans[11,3]))+.58)
  lld$fmeans[lld$number ==20 & lld$data==1]<-.22/(1+exp(-beta*fmeans[11,3]))+.58
  lld$fmeans<-as.numeric(lld$fmeans)
  lld$nll<--log(lld$fmeans)
  nll=sum(lld$nll)
  return(nll)
}
optim(c(.05), optim7, method="Brent",lower = -10, upper =10) 
#beta=0.06671538
#sum of nll= 606.7428
#AIC = 2+2nll =  1215.486


#####
#EMPIRICAL UNIVERSALIZATION
#two-parameter sigmoid
#restricting moral judgment data to only univeralizers and others
#########

####
#get only the non-uniform responders from study4a
####

one.w<-read.csv("4-7b.csv")
one.w<-one.w[complete.cases(one.w),]
two.w<-read.csv("10-13b.csv")
two.w<-two.w[complete.cases(two.w),]
one.w$condition<-"4-7"
two.w$condition<-"10-13"
moral.w<-rbind(one.w,two.w)
length(moral.w$Abaser) #n= 700 subjects in the exp


#screen out the ppl who failed the number of fishermen question
moral.w<-moral.w[moral.w$Abater==20 | moral.w$Abater==21 | moral.w$Abater==19,]
moral.w<-moral.w[moral.w$Abject==20 |moral.w$Abject==21 |moral.w$Abject==19,]
moral.w<-moral.w[moral.w$Acanth==20 | moral.w$Acanth==21| moral.w$Acanth==19,]
moral.w<-moral.w[moral.w$Accrue==20 | moral.w$Accrue==21 | moral.w$Accrue==19,]
moral.w<-moral.w[moral.w$b==20 | moral.w$b==21 | moral.w$b==19,]
moral.w<-moral.w[moral.w$i==20 | moral.w$b==21 | moral.w$b==19,] 
length(moral.w$Abaser) #615

#screen on interest
moral.w<-moral.w[moral.w$Abassi==2,]
moral.w<-moral.w[moral.w$Abider==7,]
moral.w<-moral.w[moral.w$Acajou==8,]
moral.w<-moral.w[moral.w$Accost==13,]
moral.w<-moral.w[moral.w$Achene==19,]
moral.w<-moral.w[moral.w$h==0,] 
length(moral.w$Abaser) #437

#screen on usage
moral.w<-moral.w[moral.w$Abatis==0,]
moral.w<-moral.w[moral.w$Ablaut==0,]
moral.w<-moral.w[moral.w$Acarus==0,]
moral.w<-moral.w[moral.w$Accuse==0,]
moral.w<-moral.w[moral.w$c==0,]
moral.w<-moral.w[moral.w$j==0,]
length(moral.w$Abaser) #409

#this screens out subjects who ever gave an outcome answer other than 3 (no effect of John fishing)
moral.w<-moral.w[moral.w$Abbess==3,]
moral.w<-moral.w[moral.w$Acacia==3,]
moral.w<-moral.w[moral.w$Accent==3,]
moral.w<-moral.w[moral.w$Achate==3,]
moral.w<-moral.w[moral.w$f==3,]
moral.w<-moral.w[moral.w$m==3,] 
length(moral.w$Abaser) #350

moral.w$universalizer<-NA
moral.w$permissible<-NA
moral.w$impermissible<-NA
moral.w$category<-NA #empty
moral.w$v<-NA #havent coded this yet
moral.w$other<-NA
moral.w$universalizer<-0
moral.w$permissible<-0
moral.w$impermissible<-0
moral.w$category<-0
moral.w$v<-0
moral.w$other<-0
length(moral.w$subjectcode) #350

#break subjects into categories
moral.w$universalizer[moral.w$d==0 & moral.w$k==1]<-1 #n=100 people fit this category
moral.w$permissible[moral.w$Abator==1 &
                      moral.w$Abraum==1 &
                      moral.w$Acater==1 &
                      moral.w$Acetal==1 &
                      moral.w$d==1 &
                      moral.w$k==1]<-1 #229
moral.w$impermissible[moral.w$Abator==0 &
                        moral.w$Abraum==0 &
                        moral.w$Acater==0 &
                        moral.w$Acetal==0 &
                        moral.w$d==0 &
                        moral.w$k==0]<-1  #98
moral.w$other[moral.w$universalizer==0 & moral.w$permissible==0 & moral.w$impermissible==0]<-1 #53 subjects are unaccounted for using this coding scheme
moral.w$category[moral.w$universalizer==1]<-"universalizer"
moral.w$category[moral.w$permissible==1]<-"permissible"
moral.w$category[moral.w$impermissible==1]<-"impermissible"
moral.w$category[moral.w$other==1]<-"other"

univ.w<-moral.w[moral.w$universalizer==1,] 
perm.w<-moral.w[moral.w$permissible==1,] 
imp.w<-moral.w[moral.w$impermissible==1,] 
other.w<-moral.w[moral.w$other==1,]
uo.w<-moral.w[moral.w$universalizer==1 | moral.w$other==1,] 
length(univ.w$subjectcode) #76 subjects 
length(perm.w$subjectcode) #n=192
length(imp.w$subjectcode) #n=63
length(other.w$subjectcode) #n=19
length(uo.w$subjectcode) #n=95

###format the data for graphing
#one data frame has subject answers
#the other has the number that was shown for each question
moral.ques.w<-uo.w[c(1:46,55)] #universalizers and other

#data frame with answers to questions
moral.ques<-moral.ques.w %>%
  gather(question.code,answer,-c(1:3,46:47),na.rm=TRUE) 
moral.ques$question.code = factor(moral.ques$question.code)
#character vectors to label the questions
question.list<-c(rep(c("know","interest","fishermen","usage","moral","rule","outcome"),6)) 
number.list<-c(rep(2,7),rep(7,7),rep(8,7),rep(13,7),rep(19,7),rep(0,7))
#adds the labels to the long-form data
moral.ques<-moral.ques %>%
  mutate(
    question=factor(question.list[question.code]),
    number=number.list[question.code]
  )
moral.ques<-moral.ques[c(1:5,7:9)]
moral<-moral.ques
moral<-moral[moral$question=="moral",]

#summarize data
moral.sum <- moral %>% 
  group_by(question,number,condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt((mean*(1-mean))/n)
  )

cas.w<-read.csv("CA_small.csv") 
length(cas.w$subjectcode) #n= 300 subjects in the exp

#convert to long form
cas<-cas.w %>%
  gather(question.code,answer,-c(1:3,46),na.rm=TRUE) 
cas$question.code = factor(cas$question.code)
#character vectors to label the questions
ca.list<-rep(c(rep("alleu",7),rep("johneu",7),rep("means",7)),2)
number.list<-c(rep(c(1,3,8,9,14,20,"check"),6))
threshold.list<-c(rep("4-7",21),rep("10-13",21))
#adds the labels to the long-form data
cas<-cas %>%
  mutate(
    ca.type=factor(ca.list[question.code]),
    number=number.list[question.code],
    threshold=threshold.list[question.code]
  )
cas<-cas[-c(5)]


#create exclusion flags
cas$flag<-NA
cas$flag[cas$number=="check" & cas$answer!=20]<-1


#summarize data to flag subjects to exclude
cas.exclude <- cas %>% 
  group_by(subjectcode) %>%
  summarize(
    exclude = sum(flag, na.rm=T))
#merge the exclude list with the full data set
library(plyr)
cas<-join(cas,cas.exclude,by="subjectcode", type="left")
detach("package:plyr", unload=TRUE)
#take out excluded subjects
cas<-cas[cas$exclude==0,]
cas<-cas[-c(9:10)] #data set is now clean
#remove check questions
cas<-cas[cas$number!="check",]
cas$number<-as.numeric(cas$number)

#summarize data
cas.sum <- cas %>% 
  group_by(ca.type,number,threshold) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    sd = sd(answer, na.rm = T),
    se = sd/sqrt(n)
  )

#create data frames that contain the mean response at each ip for 4,7 and 10,13 thresholds
cas.sum2<-cas.sum[c(1,2,3,5)]
alleu<-cas.sum2[cas.sum2$ca.type=="alleu",]
alleu<-alleu[-c(1)]
names(alleu)<-c("number","threshold","alleu")
johneu<-cas.sum2[cas.sum2$ca.type=="johneu",]
johneu<-johneu[-c(1)]
names(johneu)<-c("number","threshold","johneu")
fmeans<-cas.sum2[cas.sum2$ca.type=="means",]
fmeans<-fmeans[-c(1)]
names(fmeans)<-c("number","threshold","fmeans")


lld<-moral[moral$question=="moral",] 
lld<-lld[c(5,6,8)]
names(lld)<-c("threshold","data","number")
lld$number[lld$number==0]<-1
lld$number[lld$number==2]<-3
lld$number[lld$number==7]<-8
lld$number[lld$number==8]<-9
lld$number[lld$number==13]<-14
lld$number[lld$number==19]<-20
lld.all<-lld #lld.all is dataframe holding indiv subject respones to each moral judgment question


lld<-lld.all
lld$alleu<-NA

optim7<-function(para){
  lld$alleu[lld$number ==1 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*alleu[1,3]+para[2])))
  lld$alleu[lld$number ==1 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*alleu[1,3]+para[2]))
  lld$alleu[lld$number ==3 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*alleu[3,3]+para[2])))
  lld$alleu[lld$number ==3 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*alleu[3,3]+para[2]))
  lld$alleu[lld$number ==8 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*alleu[5,3]+para[2])))
  lld$alleu[lld$number ==8 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*alleu[5,3]+para[2]))
  lld$alleu[lld$number ==9 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*alleu[7,3]+para[2])))
  lld$alleu[lld$number ==9 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*alleu[7,3]+para[2]))
  lld$alleu[lld$number ==14 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*alleu[9,3]+para[2])))
  lld$alleu[lld$number ==14 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*alleu[9,3]+para[2]))
  lld$alleu[lld$number ==20 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*alleu[11,3]+para[2])))
  lld$alleu[lld$number ==20 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*alleu[11,3]+para[2]))
  lld$alleu[lld$number ==1 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*alleu[2,3]+para[2])))
  lld$alleu[lld$number ==1 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*alleu[2,3]+para[2]))
  lld$alleu[lld$number ==3 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*alleu[4,3]+para[2])))
  lld$alleu[lld$number ==3 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*alleu[4,3]+para[2]))
  lld$alleu[lld$number ==8 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*alleu[6,3]+para[2])))
  lld$alleu[lld$number ==8 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*alleu[6,3]+para[2]))
  lld$alleu[lld$number ==9 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*alleu[8,3]+para[2])))
  lld$alleu[lld$number ==9 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*alleu[8,3]+para[2]))
  lld$alleu[lld$number ==14 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*alleu[10,3]+para[2])))
  lld$alleu[lld$number ==14 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*alleu[10,3]+para[2]))
  lld$alleu[lld$number ==20 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*alleu[12,3]+para[2])))
  lld$alleu[lld$number ==20 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*alleu[12,3]+para[2]))
  lld$alleu<-as.numeric(lld$alleu)
  lld$nll<--log(lld$alleu)
  nll=sum(lld$nll)
  return(nll)
}

para=c(-3,-3)
optim(c(2,-3), optim7) 
#para[1] =   0.056508 
#para[2] = -1.164924
#sum of nll=  302.3771
#AIC = 2(2)+2nll =  608.7542


########
#Non-Empirical universalization MODEL
########

#fixed threshold and two parameter sigmoid
lld<-lld.all
lld$thresh<-NA

optim7<-function(para){
  lld$thresh[lld$number < 10 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*(1)+para[2])))
  lld$thresh[lld$number < 10 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*(1)+para[2]))
  lld$thresh[lld$number >= 10 & lld$data==0 & lld$threshold=="10-13"]<-1-(1/(1+exp(-para[1]*(0)+para[2])))
  lld$thresh[lld$number >= 10 & lld$data==1 & lld$threshold=="10-13"]<-1/(1+exp(-para[1]*(0)+para[2]))
  lld$thresh[lld$number < 4 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*(1)+para[2])))
  lld$thresh[lld$number < 4 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*(1)+para[2]))
  lld$thresh[lld$number >= 4 & lld$data==0 & lld$threshold=="4-7"]<-1-(1/(1+exp(-para[1]*(0)+para[2])))
  lld$thresh[lld$number >= 4 & lld$data==1 & lld$threshold=="4-7"]<-1/(1+exp(-para[1]*(0)+para[2]))
  lld$thresh<-as.numeric(lld$thresh)
  lld$nll<--log(lld$thresh)
  nll=sum(lld$nll)
  return(nll)
}

para=c(0,0)
optim(c(0,0), optim7) 

#para[1] =   2.3737309
#para[2] = 0.8474549
#sum of nll=  308.5323
#AIC = 2(2)+2nll =  621.0646



####
##graph
###

#for graphing model predictions against the data

#create a data frame that will hold model estimations
#put subject data in the df
model<-moral.sum[moral.sum$question=="moral",]
model<-model[c(2,3,5)] #adding col 6 gets se
model$number<-model$number+1 #adding 1 to the number to account for the fact that john would also be using the new hook
names(model)<-c("number","threshold","data")
#put ca curve values in the df, to be transformed into model estimates
cas.sum2<-cas.sum[c(1,2,3,5)]
alleu<-cas.sum2[cas.sum2$ca.type=="alleu",]
alleu<-alleu[-c(1)]
names(alleu)<-c("number","threshold","alleu")
model<-merge(model,alleu)
model$thresh<-as.numeric(model$thresh)

#transform ca values into the model estimates
model$alleu<-1/(1+exp(-0.056508 *model$alleu-1.164924))


#adding non-empirical universalization
para=c(  2.3737309, 0.8474549)
model$thresh[model$number < 10 & model$threshold=="10-13"]<-1/(1+exp(-para[1]*(1)+para[2]))
model$thresh[model$number >= 10 & model$threshold=="10-13"]<-(1/(1+exp(-para[1]*(0)+para[2])))
model$thresh[model$number < 4 & model$threshold=="4-7"]<-1/(1+exp(-para[1]*(1)+para[2]))
model$thresh[model$number >= 4 & model$threshold=="4-7"]<-(1/(1+exp(-para[1]*(0)+para[2])))


#graph
model.l<-model %>%
  gather(model,prediction,-c(1,2))


#choose any group and threshold
model.l %>%  
  #filter(threshold=="10-13")%>%
  #filter(model=="alleu" | model=="data")%>%
  #filter(model=="thresh" | model=="data")%>%
  filter(model=="thresh")%>%
  ggplot(aes(y=prediction, x=number, color=threshold, linetype=model)) + 
  geom_point()+
  geom_line() +
  coord_cartesian(ylim = c(0, 1))+
  scale_x_continuous(breaks=c(1,3,8,9,14,20))+
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(0.05))+
  theme(legend.position="right")+
  xlab("Number of interested parties (including John)")+
  ylab("P (morally acceptable)")+
  theme(legend.position="right",
        legend.text = element_text(size = "12"),
        legend.title= element_text(size="12"))+
  scale_color_manual(name="Threshold",
                     labels = c("10,13",
                                "4,7"),
                     values = c("10-13"="darkorchid", 
                                "4-7"="forestgreen"))+
  scale_linetype_manual(name="",
                        labels = c("Subject Responses",
                                   "Model Predictions"),
                        values = c("thresh" = "solid",
                                   "data" = "dashed"))+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12),
        plot.title = element_text(face="bold", size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



###########
#######
#STUDY 5
#developmental
######
########

library(tidyverse)
library(ggplot2)
library(tidyr)

data<-read.csv("data3.csv")

#remove pilot subjects
data<-data[data$subjectnumber>68,] 

length(unique(data$subjectnumber)) #n=219

#remove excluded subjects
data<-data[data$excluded==0,]

#clean data
data$condition[data$condition=="high"]<-"High"
data$condition[data$condition=="HIgh"]<-"High"
data$condition[data$condition=="low"]<-"Low"
data<-drop_na(data,subjectnumber) #one row was just NAs
data<-data[data$moral==1 | data$moral==0,] 
data$story[data$story=="buckets"]<-"Buckets"
data$story[data$story=="rocks"]<-"Rocks"

length(unique(data$subjectnumber)) #n=191

#turn story and condition into factors
data$story<-factor(data$story)
data$condition<-factor(data$condition)
data$pilotversion<-factor(data$pilotversion)


#bin the subjects by mean age
data$agebin<-NA
mean.age<-mean(data$age)
data$agebin[data$age<mean.age]<-"young" 
data$agebin[data$age>mean.age]<-"old"

data$agebin2<-NA
data$agebin2[data$age<5]<-"younger than 5" 
data$agebin2[data$age>5 & data$age<6]<-"5-6"
data$agebin2[data$age>6 & data$age<7]<-"6-7"
data$agebin2[data$age>7 & data$age<8]<-"7-8"
data$agebin2[data$age>8 & data$age<9]<-"8-9"
data$agebin2[data$age>9 & data$age<10]<-"9-10"
data$agebin2[data$age>10]<-"older than 10"

data$agebin3<-NA
data$agebin3[data$age<6]<-"younger than 6" 
data$agebin3[data$age>6 & data$age<8]<-"6-8"
data$agebin3[data$age>8]<-"older than 8"

#summarize the data by condition 
data.sum <- data %>%
  group_by(condition) %>% 
  summarize(
    n = length(moral),
    mean = mean(moral, na.rm = T), 
    se = sqrt(mean*(1-mean)/n), 
    age = mean(age)
  )

#graph the data in the "data.sum" chart
data.sum %>%
  ggplot(aes(y=mean, x=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  xlab("Condition")+
  ylab("Probability of a subject judging that\nthe action is OK")+
  ggtitle("Permissibility")

#find the chi square value for the difference between the conditions
data.table1<-table(data$moral, data$condition)
chisq.test(data.table1)


#only first stories, group by condition
data.sum2 <- data %>%
  filter(order==1)%>% #this selects only the first stories
  group_by(condition) %>%
  summarize(
    n = length(moral),
    mean = mean(moral, na.rm = T),
    se = sqrt(mean(1-mean)/n)
    #age = mean(age)
  )

#graph
data.sum2 %>%
  ggplot(aes(y=mean, x=condition)) + 
  geom_bar(position="dodge", stat="identity", fill="cornflowerblue")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  geom_text(x = 1.5, y = .8, label = "*", size=10)+
  geom_segment(aes(x = 1, y = .75, xend = 2, yend = .75))+
  xlab("Condition")+
  ylab("P (Okay)")+
  scale_fill_manual(values=c("cornflowerblue"))+
  scale_x_discrete(labels=c("High Interest", "Low Interest"))+
  #ggtitle("Permissibility \nFirst story only \nGrouped by old/young (split on mean)")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#only first stories, group by condition and age bin (young/old)
data.sum3 <- data %>%
  filter(order==1)%>%
  group_by(condition, agebin) %>%
  summarize(
    n = length(moral),
    mean = mean(moral, na.rm = T),
    se = sqrt(mean*(1-mean)/n),
    age=mean(age)
  )

#graph
data.sum3 %>%
  ggplot(aes(y=mean, x=agebin, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  xlab("Age Group")+
  ylab("P (Okay)")+
  #scale_fill_manual(values=c('#999999','cornflowerblue'))+
  scale_fill_manual(name = "Condition", 
                    labels = c("High Interest", "Low Interest"),
                    values=c('#999999','cornflowerblue'))+
  scale_x_discrete(labels=c("Older than 7.5", "Younger than 7.5"))+
  #ggtitle("Permissibility \nFirst story only \nGrouped by old/young (split on mean)")+
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", size=12, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#only first stories, group by condition and age bin2 (fine-grained age)
data$agebin2<-factor(data$agebin2)
data$agebin2 = factor(data$agebin2,levels(data$agebin2)[c(7,1:6)])
data.sum5 <- data %>%
  filter(order==1)%>%
  group_by(condition, agebin2) %>%
  summarize(
    n = length(moral),
    mean = mean(moral, na.rm = T),
    se = sqrt(mean*(1-mean)/n),
    age=mean(age)
  )


data.sum5 %>%
  ggplot(aes(y=mean, x=agebin2, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  xlab("Age group")+
  ylab("Probability of a subject judging that\nthe action is OK")+
  ggtitle("Permissibility \nFirst story only")

#only first stories, group by condition and age bin3 (less fine-grained age)
data.sum6 <- data %>%
  filter(order==1)%>%
  group_by(condition, agebin3) %>%
  summarize(
    n = length(moral),
    mean = mean(moral, na.rm = T),
    se = sqrt(mean*(1-mean)/n),
    age=mean(age)
  )

data$agebin3<-factor(data$agebin3)
data$agebin3 = factor(data$agebin3,levels(data$agebin3)[c(3,1,2)])
data.sum6 %>%
  ggplot(aes(y=mean, x=agebin3, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  xlab("Age group")+
  ylab("Probability of a subject judging that\nthe action is OK")+
  ggtitle("Permissibility \nFirst story only")


#group by story context
data.sum7 <- data %>%
  group_by(condition, story) %>%
  summarize(
    n = length(moral),
    mean = mean(moral, na.rm = T),
    se = sqrt(mean*(1-mean)/n),
    age=mean(age)
  )

data.sum7 %>%
  ggplot(aes(y=mean, x=story, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  xlab("Story")+
  ylab("Probability of a subject judging that\nthe action is OK")+
  ggtitle("Permissibility \nAll Stories")

data.sum8 <- data %>%
  group_by(condition, story, order) %>%
  summarize(
    n = length(moral),
    mean = mean(moral, na.rm = T),
    se = sqrt(mean*(1-mean)/n),
    age=mean(age)
  )

data.sum8 %>%
  filter(order==1)%>%
  ggplot(aes(y=mean, x=story, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  xlab("Story")+
  ylab("Probability of a subject judging that\nthe action is OK")+
  ggtitle("Permissibility \nFirst Stories Only")


####
##ANSWER CHANGE ANALYSIS
#####

#not all subjects answered both stories
length(unique(data$subjectnumber)) #number of subjects who answered both

data4<-data[c(1,4,17,25)]
data4<-spread(data4,condition,moral)
data4<-na.omit(data4)
data4$change<-NA
data4$change[data4$High==0 & data4$Low==1]<-"correct"
data4$change[data4$High==1 & data4$Low==0]<-"incorrect"
data4$change[data4$High==0 & data4$Low==0]<-"notOK"
data4$change[data4$High==1 & data4$Low==1]<-"OK"

expected<-data4$change[data4$change=="correct"]
unexpected<-data4$change[data4$change=="incorrect"]
notOK<-data4$change[data4$change=="notOK"]
OKay<-data4$change[data4$change=="OK"]
length(expected) #26
length(unexpected) #11
length(notOK) #74
length(OKay) #69
length(notOK)+length(OKay) #143, total nochange

# > expected
# [1] 26
# > unexpected
# [1] 11
# > nochange
# [1] 143


df<-data.frame("percent"=c(.794,.144,.061), 
               "change" = c("no change","change in expected direction","change in unexpected direction"))
df%>%
  ggplot(aes(y=percent, x=change)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  xlab("Answer Change Type")+
  ylab("Percent of Subjects")+
  ggtitle("Answer Changes Across the Conditions")


####
#ANALYSIS OF ADULT DATA
####

adult.w<-read.csv("adult.csv")
adult.w<-adult.w[adult.w$exclude==0,]
adult.w<-adult.w[c(1:43)]

#convert to long form
adult<-adult.w %>%
  gather(question.code,answer,-c(1:3,38:43),na.rm=TRUE) 
adult$question.code = factor(adult$question.code)
#character vectors to label the questions
question.list<-c("check1","check2","check3","check4","check5","moral","why1","why0",
                 "check1","check2","check3","check4","check5","check6","moral","why1","why0",
                 "check1","check2","check3","check4","check5","check6","moral","why1","why0",
                 "check1","check2","check3","check4","check5","moral","why1","why0")
context.list<-c(rep("Buckets",8),rep("Rocks",9),rep("Buckets",9),rep("Rocks",8))
condition.list<-c(rep("Low",8),rep("High",9),rep("High",9),rep("Low",8))

#adds the labels to the long-form data
adult<-adult %>%
  mutate(
    question=factor(question.list[question.code]),
    context=context.list[question.code],
    condition=condition.list[question.code]
  )
adult<-adult[-c(10)]

adult.m<-adult[adult$question=="moral",]
adult.m$answer<-as.integer(adult.m$answer)

#summarize data 
adult.sum <- adult.m %>% 
  group_by(condition) %>%
  summarize(
    n = length(answer),
    mean = mean(answer, na.rm = T),
    se = sqrt(mean*(1-mean)/n))



#find the chi square value for the difference between the conditions
adult.table<-table(adult.m$answer, adult.m$condition)
chisq.test(adult.table)

adult.sum$group<-"Adults"
data.sum2$group<-"Kids"

all.sum<-rbind(adult.sum,data.sum2)


adult.sum %>%
  #filter(context=="tour" & question=="john")%>%
  ggplot(aes(y=mean, x=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  #geom_text(x = .9, y = 1.03, label = "n.s.", size=6)+
  #geom_segment(aes(x = .65, y = 1, xend = 1.25, yend = 1))+
  #geom_text(x = 1.85, y = .9, label = "n.s.", size=16)+
  #geom_segment(aes(x = 1.5, y = .85, xend = 2.25, yend = .85))+
  xlab("Condition")+
  ylab("P (Okay)")+
  #scale_x_discrete(labels=c("Fisherman"))+
  #scale_fill_manual(name = "Condition", 
  #  labels = c("Everyone Acts","Only John Acts"),
  # values=c("gray78","gray28"))+
  #ggtitle("Motor Oil Exp; Moral Acceptability")+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#graph
all.sum %>%
  ggplot(aes(y=mean, x=group, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+  #error bars are standard error of the mean
  geom_text(x = 1, y = .93, label = "*", size=12)+
  geom_segment(aes(x = .75, y = .9, xend = 1.25, yend = .9))+
  geom_text(x = 2, y = .93, label = "*", size=12)+
  geom_segment(aes(x = 1.75, y = .9, xend = 2.25, yend = .9))+
  xlab("Group")+
  ylab("P (Okay)")+
  scale_x_discrete(labels=c("Adults","Children"))+
  scale_fill_manual(name = "Condition", 
                    labels = c("High Interest","Low Interest"),
                    values=c("#FBB4AE","#B3CDE3"))+
  theme(axis.title.x = element_text(face="bold", size=20, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=20),
        axis.title.y = element_text(face="bold", size=20, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=20),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=16),
        legend.position="right")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#prep for age-related logistic regression
adult.m2<-adult.m[c(3,4,10,12,13)]
adult.m2$context[adult.m2$context=="buckets"]<-"Buckets"
adult.m2$context[adult.m2$context=="rocks"]<-"Rocks"
adult.m2$condition[adult.m2$condition=="high"]<-"High"
adult.m2$condition[adult.m2$condition=="low"]<-"Low"

#mean age of adults
mean(adult.m2$age) #32.5

#treat all adults as if they're 18
adult.m3<-adult.m2
adult.m3$age<-18

####
#AGE ANALYSIS
####

data2 <- data %>%
  filter(order==1) #this selects only the first stories

kid<-data2[c(1,4,25,15,17)]
names(kid) <- c("subjectcode","age","answer","context","condition")
#everyone<-rbind(kid,adult.m2)
#everyone<-rbind(kid,adult.m3) #adults are all treated like 18

logit1a<- glm(moral ~ condition, data = data2, family = "binomial") 
logit1b<- glm(moral ~ condition + age, data = data2, family = "binomial") 
logit1c<- glm(moral ~ condition * age, data = data2, family = "binomial") 
summary(logit1a)
summary(logit1b)
summary(logit1c)

#to include adult data
#logit1d<- glm(answer ~ condition * age, data = everyone, family = "binomial") 
#summary(logit1d)

#graph model predictions includes adults
#everyone$predictions<-predict(logit1d,everyone,type="response")

#graph model predictions
data5<-data2[c(4,15,17,25)]
data5$predictions<-predict(logit1c,data5,type="response")

data5 %>% #NOTE: need to save this as png and set the transparency color in ppt
  ggplot(aes(y=predictions, x=age, color=condition)) + 
  geom_point()+
  coord_cartesian(ylim = c(0,1), xlim=c(4,11))+
  theme(legend.position="right")+
  xlab("Age (years)")+
  ylab("P (Okay)")+
  scale_color_manual(name = "Condition", 
                     labels = c("High Interest","Low Interest"),
                     values=c("blue","red"))+
  #ggtitle("4-7 Threshold \nSubject Moral Acceptability Data and Models")+
  theme(axis.title.x = element_text(size=14, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        axis.title.y = element_text(size=14, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#overlay
data.sum5 %>%
  ggplot(aes(y=mean, x=agebin2, fill=condition)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_cartesian(ylim = c(0, 1))+  #sets the y axis to be from 0 to 1
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9),color="black")+  #error bars are standard error of the mean
  xlab("Age group")+
  ylab("P(Okay)")+
  scale_fill_manual(name = "Condition", 
                    labels = c("High Interest","Low Interest"),
                    values=c("blue","red"))+
  #scale_fill_hue(c=45, l=80)+
  theme(axis.title.x = element_text(size=14, margin = margin(t = 20)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        axis.title.y = element_text(size=14, margin = margin(r = 20)),
        axis.text.y  = element_text(vjust=0.5, size=14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# visualizing models
#shows that the condition-only model is the best fit for the data
ggplot2::ggsave(
  plot = ggstatsplot::combine_plots(
    ggstatsplot::ggcoefstats(logit1a, title = "model-1"),
    ggstatsplot::ggcoefstats(logit1b, title = "model-2"),
    ggstatsplot::ggcoefstats(logit1c, title = "model-3"),
    nrow = 1
  ),
  filename = "model_comparison.png",
  height = 6,
  width = 16,
  units = "in",
  dpi = 300
)

####
#STORY CONTEXT ANALYSIS
####
logit2a<- glm(moral ~ condition, data = data, family = "binomial") 
logit2b<- glm(moral ~ condition + story, data = data, family = "binomial") 
logit2c<- glm(moral ~ condition * story, data = data, family = "binomial") 
summary(logit2a)
summary(logit2b)
summary(logit2c)


##
#BAYESIAN ANALYSIS
###
data2 <- data %>%
  filter(order==1) #this selects only the first stories

#all data
ggplot2::ggsave(
  plot = ggstatsplot::ggbarstats(
    data = data2,
    main = moral,
    condition = condition,
    bar.proptest = FALSE,
    ggstatsplot.layer = FALSE, # turn off ggstatsplot theme layer
    package = "wesanderson", # package from which color palette is to be taken
    palette = "Royal1", # choosing a different color palette
  ),
  filename = "means.png",
  height = 4,
  width = 5.2,
  units = "in",
  dpi = 300
)


