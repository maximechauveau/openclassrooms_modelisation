#afficher les graphiques
library(ggplot2)

#Import data
ozone <- read.table("./data/data_ozone.txt", header=TRUE, sep=";", dec=",")

#Visualisation data maxO3 en fonction de T12 Température à 12h
ggplot(ozone, aes(x=T12, y=maxO3))+
  geom_point()+
  xlab("T12")+
  ylab("maxO3")

#Régression linéaire simple
reg_simp <- lm(maxO3~T12,data=ozone)
summary(reg_simp)

#Intercept = ordonnée à l'origine (beta1)
#T12 = (beta2)
#Pr(>|t|) = la statistique de test de Student

#Visualisation avec droite de regression
ggplot(ozone,aes(x=T12,y=maxO3))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)+
  xlab("T12")+
  ylab("MaxO3")


#Création de nos valeurs ajustées
ozone$maxO3_ajust_s <- reg_simp$fit

#valeurs ajustées en fonction des valeurs observées
ggplot(ozone, aes(x=maxO3,y=maxO3_ajust_s))+
  geom_point()+
  geom_abline(intercept=0,slope=1,color="red")+
  xlab("MaxO3")+
  ylab("MaxO3 ajusté")


##Représentez les résidus du modèle

ozone$residu_s <- reg_simp$residuals

#histogramme de ces résidus

ggplot(ozone,aes(x=residu_s))+
  geom_histogram(binwidth=10,aes(y=..density..))+
  ggtitle("Histogramme")+
  xlab("Résidus")+
  ylab("")

##Prévoyons la concentration d'ozone

a_prevoir <- data.frame(T12=19)
maxO3_prev <- predict(reg_simp,a_prevoir)
round(maxO3_prev, digits=2)
