## Citizen Protection and Digitization
## Data, Figures, and Analysis

# To view the text and URLs for 40 private and 11 public organizations, visit OSF
# This .csv does not contain the text or the URLs
rep1 <- read.csv("PPPnotext.csv")
names(rep1)
attach(rep1)
str(rep1)
str(orgno)
rep1 <- subset(rep1, orgno < 41 | orgno > 100)  # Removes the Organizations from 41 through 100 which don't meet budget criteria to be in this sample.
str(rep1)
str(rep1$year)

rep2 <- subset(rep1, WC != 0)  # Removes observations for years before the organization hosted or posted a website

private <- subset(rep2, category == "private")
public <- subset(rep2, category == "public")

library(ggplot2)

# This plot shows how concerns for citizen protection change over time

oecd1 <- 
  ggplot(rep2, aes(as.numeric(year), PreventionFocus))+
  geom_point(position=position_jitter(w=.2, h=.1), alpha=.6)+
  geom_smooth(method='lm', color="firebrick")+
  facet_wrap(~category)+
  xlab("Year: 1996 to 2018")+
  ylab("Concern with Threats and Protection")+
  ggtitle("Private & Public ''About Us'' ")+
  theme_bw()+
  theme(axis.text.x = element_text(vjust=.5, angle = 90))
oecd1
ggsave(oecd1, filename = "OECDboth.png", w=5, h=4, dpi=225)

oecd2 <- 
  ggplot(public, aes(as.numeric(year), PreventionFocus))+
  geom_point(position=position_jitter(w=.2, h=.1), alpha=.6)+
  geom_smooth(method='lm', color="firebrick")+
  xlab("Year: 1996 to 2018")+
  ylab("Concern with Threats and Protection")+
  ggtitle("Government Institutions ''About Us''")+
  theme_bw()+
  theme(axis.text.x = element_text(vjust=.5, angle = 90))
oecd2
ggsave(oecd2, filename = "OECDpublic.png", w=5, h=4, dpi=225)

oecd3 <- 
  ggplot(private, aes(as.numeric(year), PreventionFocus))+
  geom_point(position=position_jitter(w=.2, h=.1), alpha=.6)+
  geom_smooth(method='lm', color="firebrick")+
  xlab("Year: 1996 to 2018")+
  ylab("Concern with Threats and Protection")+
  ggtitle("Private Defense Contractors' Mission Statements")+
  theme_bw()+
  theme(axis.text.x = element_text(vjust=.5, angle = 90))
oecd3
ggsave(oecd3, filename = "OECDprivate.png", w=5, h=4, dpi=225)

library(lme4)
library(nlme)

# 1996 subtracted from year variable in order to set a meaningful zero value

ppp1 <- lmer(PreventionFocus ~ as.numeric(year-1996) + (1|orgno), data=rep1)
ppp2 <- lmer(PreventionFocus ~ as.numeric(year-1996) + (1|orgno), data=rep2)
ppp3 <- lmer(PreventionFocus ~ as.numeric(year-1996) + (1|orgno), data=public)
ppp4 <- lmer(PreventionFocus ~ as.numeric(year-1996) + (1|orgno), data=private)

# testing for an interaction by private/public category
ppp5 <- lmer(PreventionFocus ~ as.numeric(year-1996) * category + (1|orgno), data=rep2)

summary(ppp1)
summary(ppp2)
summary(ppp3)
summary(ppp4)
summary(ppp5)

# Same tests; different R package

ppp1.1 <- lme (PreventionFocus ~ as.numeric(year-1996), random=~1|orgno, na.action=na.exclude, data=rep1)
ppp2.1 <- lme (PreventionFocus ~ as.numeric(year-1996), random=~1|orgno, na.action=na.exclude, data=rep2)
ppp3.1 <- lme (PreventionFocus ~ as.numeric(year-1996), random=~1|orgno, na.action=na.exclude, data=public)
ppp4.1 <- lme (PreventionFocus ~ as.numeric(year-1996), random=~1|orgno, na.action=na.exclude, data=private)
ppp5.1 <- lme (PreventionFocus ~ as.numeric(year-1996) * category , random=~1|orgno, na.action=na.exclude, data=rep2)

summary(ppp1.1)
summary(ppp2.1)
summary(ppp3.1)
summary(ppp4.1)
summary(ppp5.1)
