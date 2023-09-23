#CUTTOFF DATAFRAME ON POPULATION OF STUDY, BEGINNING AND CONCLUSION TREATMENT, AND MAIN OUTCOME

setwd("C:\\Users\\osinu\\OneDrive\\Área de Trabalho\\Pós-graduação\\Mestrado\\Dissertação\\Analises_dissertacao")

require(foreign)
require(tidyverse)
library(dplyr)

banco_1 <-read.csv("banco_final.csv")

#EXPLORATORY REVIEW
glimpse(banco_1)
dim(banco_1)
head(banco_1,1)

summary(as.Date(banco_1$dt_inic_tr))
summary(as.Date(banco_1$dt_encerra))
summary(as.Date(banco_1$dt_notific))

table(banco_1$nu_ano)

table(banco_1$pop_rua,useNA = "always")
unique(banco_1$pop_rua)


#primeiro recorte:selecionando apenas pop rua e não rua (1,2) 
#first cut: selecting only homeless and not homeless (1,2)

banco_2<- banco_1%>%
  filter(pop_rua==1|pop_rua==2)

table(banco_2$pop_rua,useNA = "always")



#SEGUNDO CORTE: DATA MÍNIMA EM 01/01/2015
#SECOND CUT: MINIMUM DATE TO 01-01-2015

table(banco_2$dt_inic_tr)
summary(banco_2$dt_inic_tr)
summary(as.Date(banco_2$dt_inic_tr))

banco_3 <-banco_2%>%
  filter((as.Date(dt_inic_tr)>as.Date("2014-12-31")))

summary(as.Date(banco_3$dt_inic_tr))


#TERCEIRO CORTE: "inicio de tratamento" mínimo abaixo de 01/01/2021
#THIRD CUT: MINIMUM of "inicio do tratamento" on DATE under 01/01/2021

banco_4 <-banco_3%>%
  filter((as.Date(dt_inic_tr)<as.Date("2021-01-01")))

summary(as.Date(banco_4$dt_inic_tr))



#inspecionando a data de encerramento do tratamento
#inspecting date of closure treatment
summary(as.Date(banco_4$dt_encerra))




#diferença entre encerramento - inicio do tratamento para ter datas positivas
#difference between closure - begining of treatment to achive positive dates

banco_4$dif_encerr_inic<-as.Date(banco_4$dt_encerra) - as.Date(banco_4$dt_inic_tr)
banco_4$dif_encerr_inic<-as.numeric(banco_4$dif_encerr_inic)

str(banco_4$dif_encerr_inic)


#closure treatment date is after begining?
summary(banco_4$dif_encerr_inic)
hist((as.numeric(banco_4$dif_encerr_inic)))

#aqui vemos que existem datas negativas e outras muito grandes. Vamos recortar
#here we seen that there are negative dates and very long ones. So, lets cut it again


#cortando a data de diferença par ser >0
#cutting diference betwenn the >0
banco_5 <-banco_4%>%
  filter(dif_encerr_inic>0)

summary(banco_5$dif_encerr_inic) 

summary(as.Date(banco_5$dt_encerra))


#cortando em um novo banco, considerando a data máxima da diferença em 31/12/2020
#cutting with new data frame, considering maximum date on 12/31/2020

banco_6 <-banco_5%>%
  filter((as.Date(dt_encerra)<as.Date("2021-01-01")))

summary(as.Date(banco_6$dt_encerra))
summary(banco_6$dif_encerr_inic)

banco_6%>%
  ggplot(aes(x=dif_encerr_inic))+
  geom_histogram()


banco_6%>%
  ggplot(aes(x=dif_encerr_inic))+
  geom_density()


summary(as.Date(banco_6$dt_encerra))
summary(as.numeric(banco_6$dif_encerr_inic))






#selecionando pessoais que estiveram em 180 dias de tratamento
#select only people who were on treatment until 180days

banco_6 <-banco_6%>%
  filter(dif_encerr_inic<271)
  
  
#filter(dif_encerr_inic<181)
#filter(dif_encerr_inic<271)
#filter(dif_encerr_inic<361)

summary(banco_6$dif_encerr_inic)

banco_6%>%
  ggplot(aes(x=dif_encerr_inic))+
  geom_histogram()







#SELEÇÃO DA IDADE
#AGE SELECTION

str(banco_6$nu_idade_n)

banco_6%>%
  ggplot(aes(x=nu_idade_n, color = pop_rua))+
  geom_density()


banco_6%>%
  ggplot(aes(x=nu_idade_n,color = pop_rua))+
  geom_boxplot()+
  coord_flip()

summary(banco_6$nu_idade_n)


#filter peoplo only up to 18 years old
#filtrando por idade acima de 18 anos

str(banco_6$nu_idade_n)

banco_7 <-banco_6%>%
  filter((as.integer(nu_idade_n)>as.integer("4017")))

summary(as.integer(banco_7$nu_idade_n))
summary(as.integer(banco_7$nu_idade_n[banco_7$pop_rua==1]))
summary(as.integer(banco_7$nu_idade_n[banco_7$pop_rua==2]))

table(banco_7$nu_idade_n[banco_7$pop_rua==1])
table(banco_7$nu_idade_n[banco_7$pop_rua==2])

#graficos
#graphs
banco_7%>%
  ggplot(aes(x=nu_idade_n, color = pop_rua))+
  geom_histogram()

banco_7%>%
  ggplot(aes(x=nu_idade_n, color = pop_rua))+
  geom_density()


banco_7%>%
  ggplot(aes(x=nu_idade_n,color = pop_rua))+
  geom_boxplot()+
  coord_flip()




#filtrando por idade abaixo de 90 anos
#filter for age under 90's
str(banco_7$nu_idade_n)

banco_8 <-banco_7%>%
  filter((as.integer(nu_idade_n)<as.integer("4091")))

summary(as.integer(banco_8$nu_idade_n))



#graficos
#graphs
banco_8%>%
  ggplot(aes(x=nu_idade_n, color = pop_rua))+
  geom_histogram()

banco_8%>%
  ggplot(aes(x=nu_idade_n, color = pop_rua))+
  geom_density()


banco_8%>%
  ggplot(aes(x=nu_idade_n,color = pop_rua))+
  geom_boxplot()+
  coord_flip()


#retirando dois primeiros digitos
#removing two first digits


banco_8$idade2_nova<-as.numeric(str_sub(as.character(banco_8$nu_idade_n),
                                        start = 3L, end = -1L))
str(banco_8$idade2_nova)
summary(banco_8$idade2_nova)

banco_8%>%
  ggplot(aes(x=idade2_nova, color = pop_rua))+
  geom_histogram()






#seleção de observações para coorte a partir da variavel caso novo
#selection of observations to cohort from variable new case
table(banco_8$tratamento, useNA = "always")


#filtrando por caso novo (caso novo 1)
#filter to new case (new case 1) 

banco_8 <-banco_8%>%
  filter(tratamento==1)



#remoção de notificações duplicadas
#removing duplicated notifications

banco_9<-banco_8 %>% distinct(dt_notific, dt_nasc, dt_inic_tr,cs_sexo,forma,sg_uf_not,
                              id_municip,tratamento,situa_ence,dt_encerra,nu_ano, .keep_all = TRUE)


table(banco_9$situa_ence[banco_9$pop_rua==1],useNA = "always")
table(banco_9$situa_ence[banco_9$pop_rua==2],useNA = "always")


glimpse(banco_9)


#novo banco com categorias que formam sucesso e insucesso
#new data frame with categories that form success and unsuccess

banco_9<-banco_9%>% 
  mutate(desfecho1 =case_when((situa_ence=="1" )|(situa_ence==" 1")|(situa_ence=="01")~"g1.cura",
                              (situa_ence=="2")|(situa_ence==" 2")|(situa_ence=="02")~"g2.abandono",
                              (situa_ence=="3")|(situa_ence==" 3")|(situa_ence=="03")~"g3.óbito_TB",
                              (situa_ence=="4")|(situa_ence==" 4")|(situa_ence=="04")~"g4.óbito_outra_causa",
                              (situa_ence=="5")|(situa_ence==" 5")|(situa_ence=="05")~"g5.transferencia",
                              (situa_ence=="7")|(situa_ence==" 7")|(situa_ence=="07")~"g7.TB_DR",
                              (situa_ence=="8")|(situa_ence==" 8")|(situa_ence=="08")~"g8.Mudanca_esquema",
                              (situa_ence=="9")|(situa_ence==" 9")~"g9.falencia",
                              (situa_ence=="10")~"g10.abandono_pri",
                              (situa_ence=="")|(situa_ence==".")~"g11.miss"))

banco_9$desfecho1[is.na(banco_9$desfecho1)==TRUE]<-"g11.miss"
table(banco_9$desfecho1,useNA = "always")


banco_9<- banco_9%>%
  filter(desfecho1=="g1.cura"|desfecho1=="g2.abandono"|desfecho1=="g3.óbito_TB"|
           desfecho1=="g4.óbito_outra_causa"|desfecho1=="g9.falencia"|desfecho1=="g11.miss"|desfecho1=="g5.transferencia")


table(banco_9$desfecho1,useNA = "always")

summary(banco_9$dif_encerr_inic[banco_9$desfecho1=="g1.cura"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho1=="g2.abandono"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho1=="g3.óbito_TB"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho1=="g4.óbito_outra_causa"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho1=="g9.falencia"])




#criando a variavel desfecho do tratamento como preconiza a OMS
#creating variable treatment outcome as OMS suggests

banco_9<-banco_9%>% 
  mutate(desfecho2 =case_when(desfecho1=="g1.cura"~"g1.sucsess",
                              (desfecho1=="g2.abandono")~"g2.loss_follow_up",
                              (desfecho1=="g3.óbito_TB")|(desfecho1=="g4.óbito_outra_causa")~"g3.death_geral",
                              (desfecho1=="g9.falencia")~"g4.failure",
                              (desfecho1=="g5.transferencia")|(desfecho1==" g11.miss")~"g5.Not_evaluated"))

banco_9$desfecho2[is.na(banco_9$desfecho2)==TRUE]<-"g5.Not_evaluated"

table(banco_9$desfecho2,useNA = "always")
(table(banco_9$desfecho2,useNA = "always")/length(banco_9$desfecho2))*100


summary(banco_9$dif_encerr_inic[banco_9$desfecho2=="g1.sucsess"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho2=="g2.loss_follow_up"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho2=="g3.death_geral"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho2=="g4.failure"])
summary(banco_9$dif_encerr_inic[banco_9$desfecho2=="g5.Not_evaluated"])

banco_9%>%
  ggplot(aes(x=dif_encerr_inic, color = desfecho2))+
  geom_density()


summary(as.Date(banco_9$dt_encerra)[banco_9$desfecho2=="g1.sucsess"])
summary(as.Date(banco_9$dt_encerra)[banco_9$desfecho2=="g2.loss_follow_up"])
summary(as.Date(banco_9$dt_encerra)[banco_9$desfecho2=="g3.death_geral"])
summary(as.Date(banco_9$dt_encerra)[banco_9$desfecho2=="g4.failure"])








#o período que está trabalhando está certo? 
#is the period im workin correct?
summary(as.Date(banco_9$dt_encerra))
summary(as.Date(banco_9$dt_inic_tr))
summary(as.Date(banco_9$dt_notific))
summary(banco_9$dif_encerr_inic)
table(banco_9$nu_ano)

#Sim, está//Yes , it is





banco_9$pop_rua_1 <-banco_9$pop_rua
table(banco_9$pop_rua_1, useNA = "always")

#rotulando variavel nova
#labeling new variable
banco_9$pop_rua_1 <- factor(banco_9$pop_rua_1, levels = c(1,2),
                            labels = c("g.2 Sim", "g.1 Não"))

#conferindo rotulo
table(banco_9$pop_rua_1, useNA = "always")
(table(banco_9$pop_rua_1,useNA = "always")/length(banco_9$pop_rua_1))*100



#PROPORÇÃO DOS DESFECHOS / OUTCOME PROPORTION

#POP_RUA
#HOMELESS
table(banco_9$desfecho2[banco_9$pop_rua==1], useNA = "always") 
(table(banco_9$desfecho2[banco_9$pop_rua==1],useNA = "always")/length(banco_9$desfecho2[banco_9$pop_rua==1]))*100


#NAO RUA
#NOT HOMELESS
table(banco_9$desfecho2[banco_9$pop_rua==2], useNA = "always") 
(table(banco_9$desfecho2[banco_9$pop_rua==2],useNA = "always")/length(banco_9$desfecho2[banco_9$pop_rua==2]))*100





################################################
#    HARMONIZAÇÃO DAS VARIAVEIS-HARMONIZING    #
#                                              #
#                                              #
################################################





#---------------------#
#HARMONIZANDO ANO/YEAR#
#---------------------#

table(banco_9$nu_ano,useNA = "always")
banco_9$ANO_1<-banco_9$nu_ano
table(banco_9$ANO_1,useNA = "always")




#----------------------#
#HARMONIZANDO SEXO/SEX #
#----------------------#
table(banco_9$cs_sexo,useNA = "always")
unique(banco_9$cs_sexo)
banco_9$sexo1 <-banco_9$cs_sexo
table(banco_9$sexo1, useNA = "always")

#rotulando
#labeling
banco_9$sexo1<-factor(banco_9$sexo1,
                      levels = c("F","M","I"),
                      labels = c("g.1 Feminino","g.2 Masculino", "g.3 Ignorado"))


table(banco_9$sexo1, useNA = "always")  




#----------------------#
#Harmonizando raca/race#
#----------------------#
table(banco_9$cs_raca,useNA = "always")
unique(banco_9$cs_raca)


#variavel nova/ new variable
banco_9$raca_1 <-banco_9$cs_raca
table(banco_9$raca_1,useNA = "always")

banco_9$raca_1 <- factor(banco_9$raca_1, levels = c(1,2,3,4,5,9),
                         labels = c("g.1Branca", "g.2Preta","g.3Amarela","g.4Parda",
                                    "g.5Indígena", "g.6 Ignorado"))

banco_9$raca_1[is.na(banco_9$raca_1)==TRUE]<-"g.6 Ignorado"

#deu certo? it works?
table(banco_9$raca_1, useNA = "always")

#----------------------------------------------#
#CRIANDO VARIAVEL RACA NEGRA (PRETOS E PARDOS/ #
#CREATING VARIABEL BLACK RACE (BLACK AND BROWN)#
#----------------------------------------------#

library(dplyr)
banco_9<-banco_9%>%
  mutate(raca_2 =case_when(raca_1=="g.1Branca"~"g.1Brancos", raca_1=="g.2Preta"|raca_1=="g.4Parda"~"g.2Negros",
                           raca_1=="g.3Amarela"~"g.3Amarelos",raca_1=="g.5Indígena"~"g.4indígena", raca_1=="g.6 Ignorado"~"g.5ignorados"))


#deu certo? it works?
table(banco_9$raca_2,useNA = "always")





#-------------------------------------#
#Harmonizando escolaridade/scholarship#
#-------------------------------------#
table(banco_9$cs_escol_n,useNA = "always")
unique(banco_9$cs_escol_n)

banco_9<-banco_9%>% 
  mutate(escolaridade_1 =case_when((cs_escol_n=="0")~"g0.anal",
                                   (cs_escol_n=="1")~"g1.1_4º",
                                   (cs_escol_n=="2")~"g2.4ºserie",
                                   (cs_escol_n=="3")~"g3.5_8ºserie",
                                   (cs_escol_n=="4")~"g4.8ºserie",
                                   (cs_escol_n=="5")~"g5.medio_incomp",
                                   (cs_escol_n=="6")~"g6.medio_completo",
                                   (cs_escol_n=="7")~"g7.superio_incom",
                                   (cs_escol_n=="8")~"g8.superior_completo"))

banco_9$escolaridade_1[is.na(banco_9$escolaridade_1)==TRUE]<-"g.9 Ignorado"

table(banco_9$escolaridade_1, useNA = "always")



#categorizacao da escolaridade
#categorizing scholarship

unique(banco_9$escolaridade_1)

banco_9<-banco_9%>%
  mutate(escolaridade2 = case_when((escolaridade_1=="g0.anal")~"g4.analfabetos", 
                                   (escolaridade_1=="g1.1_4º")|(escolaridade_1=="g2.4ºserie")|(escolaridade_1=="g3.5_8ºserie")|(escolaridade_1=="g4.8ºserie")~"g3.Ens.Fund.I-II",
                                   (escolaridade_1=="g5.medio_incomp")|(escolaridade_1=="g6.medio_completo")~"g2.Ens.Medio",
                                   (escolaridade_1=="g7.superio_incom")|(escolaridade_1=="g8.superior_completo")~"g1.Ens.Superior_ou_mais",
                                   (escolaridade_1=="g.9 Ignorado")~"g5.missing"))


table(banco_9$escolaridade2,useNA = "always")


#-------------------------------------#
#Harmonizando trans renda/cash transfe#
#-------------------------------------#

table(banco_9$benef_gov,useNA = "always")
unique(banco_9$benef_gov)

banco_9 <-banco_9%>%
  mutate(benefic_trans_renda= case_when((benef_gov=="1")~"g2.Sim",
                             (benef_gov=="2")~"g1.Não",
                             (benef_gov=="9")|(benef_gov=="5")|(benef_gov=="7")|(benef_gov=="")~"g3.ignorado"))


table(banco_9$benefic_trans_renda,useNA = "always")


#------------------------------#
#Harmonizando os estados/states#
#------------------------------#
table(banco_9$sg_uf_not,useNA = "always")
banco_9$estados <-banco_9$sg_uf_not
table(banco_9$estados,useNA = "always")
banco_9$estados <-factor(banco_9$estados,
                         levels = c(11,12,13,14,15,16,17,21,22,23,24,
                                    25,26,27,28,29,31,32,33,35,41,42,43,
                                    50,51,52,53),
                         labels = c("RO","AC","AM","RR","PA","AP","TO",
                                    "MA","PI","CE","RN","PB","PE","AL",
                                    "SE","BA","MG","ES","RJ","SP","PR",
                                    "SC","RS","MS","MT","GO","DF"))

anyNA(is.na(banco_9$estados))

table(banco_9$estados,useNA = "always")
(table(banco_9$estados,useNA = "always")/length(banco_9$estados))*100

#CATEGORIZANDO ESTADOS EM MACRORREGIOES
#CATEGORIZING STATES IN MACRO-REGIONS

unique(banco_9$estados)

banco_9<-banco_9%>%
  mutate(regioes_BR =case_when((estados=="AC")|(estados=="AM")|(estados=="RO")|(estados=="RR")|(estados=="AP")|(estados=="PA")|(estados=="TO")~"g.2 Norte",
                               (estados=="MA")|(estados=="PI")|(estados=="CE")|(estados=="RN")|(estados=="PB")|(estados=="PE")|(estados=="AL")|(estados=="SE")|(estados=="BA")~"g.3 Nordeste",
                               (estados=="MS")|(estados=="MT")|(estados=="GO")|(estados=="DF")~"g.4 Centro_Oeste",
                               (estados=="MG")|(estados=="ES")|(estados=="RJ")|(estados=="SP")~"g.1 Sudestes",
                               (estados=="PR")|(estados=="SC")|(estados=="RS")~"g.5 Sul"))

table(banco_9$regioes_BR,useNA = "always")
(table(banco_9$regioes_BR,useNA = "always")/length(banco_9$regioes_BR))*100




#------------------------#
#Harmonizacao  capitais  #
#Harmonization capitals  #
#------------------------#
table(banco_9$id_municip,useNA = "always")
anyNA(is.na(banco_9$id_municip))

banco_9$capitais <-banco_9$id_municip

#ajustes na variavel nova

table(banco_9$capitais,useNA = "always")
banco_9$capitais <-factor(banco_9$capitais,
                          levels = c(120040,270430,160030,130260,292740,230440,530010,
                                     320530,520870,211130,510340,500270,310620,150140,250750,
                                     410690,261160,221100,240810,431490,330455,110020,140010,
                                     420540,355030,280030,172100),
                          labels = c("g.1Rio Branco","g2.Maceio","g3.Macapá","g4.Manaus","g5.Salvador",
                                     "g6.Fortaleza","g.7Brasilia", "g8.Vitoria","g9.Goiania","g10.Sao Luis",
                                     "g11.Cuiaba","g12.Campo Grande","g13.Belo Horizonte","g14.Belem","g15.Joao Pessoa","g16.Curitiba",
                                     "g17.Recife","g18.Teresina","g19.Natal","g20.Porto Alegre","g21.Rio de Janeiro","g22.Porto Velho",
                                     "g23.Boa Vista","g.24Floripa","g25.São Paulo","g26.Aracaju","g.27Palmas"))


anyNA(is.na(banco_9$capitais))

table(banco_9$capitais,useNA = "always")

banco_9$capitais[is.na(banco_9$capitais)==TRUE]<-"g28.Ignorado"



#--------------------------------------------------------#
#Harmonizando forma da tuberculose/ TYPO OF TUBERCULOSIS #
#--------------------------------------------------------#

table(banco_9$forma, useNA = "always")
banco_9$forma_TB <-banco_9$forma 
table(banco_9$forma_TB, useNA = "always") 
unique(banco_9$forma_TB)

banco_9$forma_TB <- factor(banco_9$forma_TB, levels = c(1,2,3),
                           labels = c("g.2 Pulmonar", "g.1 Extrapulmonar", "g.3 Pulmo+extra"))

anyNA(is.na(banco_9$forma_TB))

table(banco_9$forma_TB,useNA = "always")
(table(banco_9$forma_TB,useNA = "always")/length(banco_9$forma_TB))*100


#agrupando pulmonar com extra+pulmonar (OMS,2020)
#grouping pulmonary with extra+pulmo (WH0,2020)

banco_9<-banco_9%>%
  mutate(forma_clinica2 =case_when(forma_TB=="g.2 Pulmonar"|forma_TB=="g.3 Pulmo+extra"~"g2.Pulmonar",
                                   forma_TB=="g.1 Extrapulmonar"~"g1.extrapulmonar"))


table(banco_9$forma_clinica2,useNA = "always")
(table(banco_9$forma_clinica2,useNA = "always")/length(banco_9$forma_clinica2))*100





#----------------------------------------------------#
#Harmonizando tipo de entrada/ registration on system#
#----------------------------------------------------#

unique(banco_9$tratamento)
table(banco_9$tratamento,useNA = "always")
banco_9$tipo_entrada<-banco_9$tratamento
table(banco_9$tipo_entrada,useNA = "always")

#ROTULANDO/LABELING

banco_9$tipo_entrada <-factor(banco_9$tipo_entrada,
                              levels = c(1),
                              labels = c("g.1 Caso_novo"))


table(banco_9$tipo_entrada,useNA = "always")
(table(banco_9$tipo_entrada,useNA = "always")/length(banco_9$tipo_entrada))*100


#-----------------#
#HARMONIZANDO HIV #
#-----------------#

table(banco_9$hiv,useNA = "always")
unique(banco_9$hiv)

#VARIAVEL NOVA / NEW VARIABLE

banco_9<-banco_9%>%
  mutate(HIV_1 = case_when((hiv == "1")~"g.2 Coinfecção",
                           (hiv == "2")|(hiv == "3")|(hiv == "4")~"g.1 Ñ_coinfecção"))


banco_9$HIV_1[is.na(banco_9$HIV_1)==TRUE]<-"g.3 Ignorado"






#--------------------------------#
#HARMINIZANDO COMORBIDADE ALCOOL/#
#HARMONIZATION COMORBITY ALCHOOL #
#--------------------------------#
table(banco_9$agravalcoo,useNA = "always")
unique(banco_9$agravalcoo)

#ROTULANDO/LABELING

banco_9<-banco_9%>%
  mutate(ALCOOL_1 =case_when((agravalcoo=="1")~"g.2 Sim",
                             (agravalcoo=="2")~"g.1 Não",
                             (agravalcoo=="9")~"g.3 Ignorado"))


banco_9$ALCOOL_1[is.na(banco_9$ALCOOL_1)==TRUE]<-"g.3 Ignorado"




#----------------------#
#HARMONIZANDO diabetes #
#HARMONIZATION diabetes#
#----------------------#

table(banco_9$agravdiabe,useNA = "always")
unique(banco_9$agravdiabe)

#ROTULANDO/LABELING

banco_9<-banco_9%>%
  mutate(DIABETES =case_when((agravdiabe=="1")~"g.2 Sim",
                             (agravdiabe=="2")~"g.1 Não",
                             (agravdiabe=="9")~"g.3 Ignorado"))


banco_9$DIABETES[is.na(banco_9$DIABETES) == TRUE]<-"g.3 Ignorado"



#-------------------------------------------#
#HARMONIZANDO doenca mental /MENTAL DISORDER#
#-------------------------------------------#
table(banco_9$agravdoenc,useNA = "always")
unique(banco_9$agravdoenc)


#ROTULANDO/LABELING

banco_9<-banco_9%>%
  mutate(Doenca_mental =case_when((agravdoenc=="1")~"g.2 Sim",
                                  (agravdoenc=="2")~"g.1 Não",
                                  (agravdoenc=="9")~"g.3 Ignorado"))


banco_9$Doenca_mental[is.na(banco_9$Doenca_mental) == TRUE]<-"g.3 Ignorado"




#-------------------#
#HARMONIZANDO TABACO#
#-------------------#

table(banco_9$agravtabac,useNA = "always")
unique(banco_9$agravtabac)

#ROTULANDO/LABELING

banco_9<-banco_9%>%
  mutate(TABACO_1 =case_when((agravtabac=="1")~"g.2Sim",
                             (agravtabac=="2")~"g.1Não",
                             (agravtabac=="9")~"g.3Ignorado"))


banco_9$TABACO_1[is.na(banco_9$TABACO_1) == TRUE]<-"g.3Ignorado"




#------------------------------------------#
#HARMONIZANDO DROGA_ilicita/ ILICIT DROGUES#
#------------------------------------------#

table(banco_9$agravdroga,useNA = "always")
unique(banco_9$agravdroga)

#ROTULANDO/LABELING

banco_9<-banco_9%>%
  mutate(DROGA_ILICITA =case_when((agravdroga=="1")~"g.2Sim",
                                  (agravdroga=="2")~"g.1Não",
                                  (agravdroga=="9")~"g.3Ignorado"))

banco_9$DROGA_ILICITA[is.na(banco_9$DROGA_ILICITA) == TRUE]<-"g.3Ignorado"




#--------------------------------------------#
#HARMONIZANDO TDO/ DIRECT  OBSERVED TREATMENT#
#--------------------------------------------#

table(banco_9$tratsup_at,useNA = "always")

#ROTULANDO/LABELING

banco_9<-banco_9%>%
  mutate(TDO_1 =case_when((tratsup_at=="1")~"g.1Sim",
                          (tratsup_at=="2")~"g.2Não",
                          (tratsup_at=="9")~"g.3Ignorado"))


banco_9$TDO_1[is.na(banco_9$TDO_1)==TRUE]<-"g.3Ignorado"


#DESCRITIVA UNIVARIADA
table(banco_9$TDO_1,useNA = "always")
(table(banco_9$TDO_1,useNA = "always")/length(banco_9$TDO_1))*100



#-----------------------#
#harmonizando idade/ AGE#   
#-----------------------#   



#descritiva por populacao / descritive for population

summary(banco_9$idade2_nova[banco_9$pop_rua_1=="g.2 Sim"])

summary(banco_9$idade2_nova[banco_9$pop_rua_1=="g.1 Não"])



#categorizacao da idade em quatro categorias
#categorizing age in four categories

#o sinal de "&" significa "e / mais"
#sign "&" means "and / plus"

banco_9<-banco_9%>% mutate(idade_categ =case_when((idade2_nova>=18)&(idade2_nova<=29)~"g1.18a29",
                                                  (idade2_nova>=30)&(idade2_nova<=39)~"g2.30a39",
                                                  (idade2_nova>=40)&(idade2_nova<=49)~"g3.40a49",
                                                  (idade2_nova>=50)&(idade2_nova<=59)~"g4.50a59",
                                                  (idade2_nova>=60)&(idade2_nova<=69)~"g5.60a69",
                                                  (idade2_nova>=70)&(idade2_nova<=90)~"g6.70a90"))
#DESCRITIVA UNIVARIADA
table(banco_9$idade_categ,useNA="always")
(table(banco_9$idade_categ,useNA="always")/length(banco_9$idade_categ))*100



#saving modified data frame for next script on csv format
#salvando banco modificado para próximo script em formato csv

write.csv(banco_9,"dissertacao_survival_1.csv")




################################################
#    ANÁLISE DESCRITIVA-DESCRIPTIVE ANALYSIS   #
#                                              #
#                                              #
################################################


banco_9 <-read.csv("dissertacao_survival_1.csv")


#------------------#
#DESCRITIVA POP RUA#
#------------------#

#DESCRITIVA UNIVARIADA
table(banco_9$pop_rua_1,useNA = "always")
(table(banco_9$pop_rua_1,useNA = "always")/length(banco_9$pop_rua_1))*100



#---------------------#
#DESCRITIVA POR ANO   #
#---------------------#

#---------------------#
#frequencias pop n-rua#
#---------------------#
table(banco_9$ANO_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")
(table(banco_9$ANO_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$ANO_1[banco_9$pop_rua_1=="g.1 Não"]))*100


#---------------------#
#frequencias pop rua  #
#---------------------#
table(banco_9$ANO_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")
(table(banco_9$ANO_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$ANO_1[banco_9$pop_rua_1=="g.2 Sim"]))*100



#---------------#
#DESCRITIVA SEXO#
#---------------#
#---------------------#
#frequencias pop n-rua#
#---------------------#

#ABSOLUTA e RELATIVA
table(banco_9$sexo1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")
(table(banco_9$sexo1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$sexo1[banco_9$pop_rua_1=="g.1 Não"]))*100



#---------------------#
#frequencias pop rua  #
#---------------------#

#ABSOLUTA e RELATIVA
table(banco_9$sexo1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")
(table(banco_9$sexo1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$sexo1[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TESTE DE HIPOTESE with POP_RUA/ HIPOTHESIS TEST WITH POP_RUA
#TABELA DE CONTINGENCIA DE SEXO E POPRUA_1/ CONTINGENCY TABLE:

tabela_sexo_poprua <-table(banco_9$sexo1,banco_9$pop_rua_1)
tabela_sexo_poprua

#TESTE QUI-QUADRADO (X²) ENTRE SEXO E POPRUA_1/ CHI-SQUARE SEX:

quiqua_sexo_poprua <-chisq.test(tabela_sexo_poprua)
quiqua_sexo_poprua

chisq.test(tabela_sexo_poprua)$expected

#valor esperado menor que 5, vamos usar o teste de fisher
#expected value minor to 5, lets use fisher's test


#aplicando teste de Fisher /applying fisher's test

fisher_sexo <- fisher.test(tabela_sexo_poprua)
fisher_sexo

fisher_sexo$p.value

fisher.test(table(banco_9$sexo1,banco_9$pop_rua_1))

fisher.test(tabela_sexo_poprua,simulate.p.value=TRUE,B=1e7)


#-----------------#
#DESCRITIVA RACA_1#
#-----------------#

#UNIVARIADA
table(banco_9$raca_1, useNA = "always")
(table(banco_9$raca_1,useNA = "always")/length(banco_9$raca_1))*100


#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$raca_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")
(table(banco_9$raca_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$raca_1[banco_9$pop_rua_1=="g.1 Não"]))*100 

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$raca_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")
(table(banco_9$raca_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$raca_1[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TESTE DE HIPOTESE with POP_RUA/ HIPOTHESIS TEST WITH POP_RUA
#TABELA DE CONTINGENCIA DE RACA E POPRUA_1/ CONTINGENCY TABLE:

tabela_raca_poprua <-table(banco_9$raca_1,banco_9$pop_rua_1)
tabela_raca_poprua

#TESTE QUI-QUADRADO (X²) ENTRE RACA E POPRUA_1/ CHI-SQUARE RACA:

quiqua_raca_poprua <-chisq.test(tabela_raca_poprua)
quiqua_raca_poprua

chisq.test(tabela_raca_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_raca_poprua





#-----------------#
#DESCRITIVA RACA_2#
#-----------------#
#UNIVARIADA
#FREQUENCIAS TOTAIS
table(banco_9$raca_2,useNA = "always")
(table(banco_9$raca_2,useNA = "always")/length(banco_9$raca_2))*100



#BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#
#ABSOLUTA e RELATIVA
table(banco_9$raca_2[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")
(table(banco_9$raca_2[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$raca_2[banco_9$pop_rua_1=="g.1 Não"]))*100 


#---------------------#
#frequencias pop rua  #
#---------------------#

#ABSOLUTA e RELATIVA
table(banco_9$raca_2[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")
(table(banco_9$raca_2[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$raca_2[banco_9$pop_rua_1=="g.2 Sim"]))*100


#TESTE DE HIPOTESE with POP_RUA/ HIPOTHESIS TEST WITH POP_RUA
#TABELA DE CONTINGENCIA DE RACA E POPRUA_1/ CONTINGENCY TABLE:

tabela_raca2_poprua <-table(banco_9$raca_2,banco_9$pop_rua_1)
tabela_raca2_poprua

#TESTE QUI-QUADRADO (X²) ENTRE RACA E POPRUA_1/ CHI-SQUARE RACA:

quiqua_raca2_poprua <-chisq.test(tabela_raca2_poprua)
quiqua_raca2_poprua

chisq.test(tabela_raca2_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_raca2_poprua




#------------------------#
#DESCRITIVA FX_ETARIA    #
#------------------------#

#DESCRITIVA UNIVARIADA
table(banco_9$idade_categ,useNA="always")
(table(banco_9$idade_categ,useNA="always")/length(banco_9$idade_categ))*100


#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$idade_categ[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$idade_categ[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$idade_categ[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$idade_categ[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$idade_categ[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$idade_categ[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TABELA DE CONTINGENCIA DE IDADE E POPRUA_1/ CONTINGENCY TABLE AGE:

tabela_idade_categ_poprua <-table(banco_9$idade_categ,banco_9$pop_rua_1)
tabela_idade_categ_poprua

#TESTE QUI-QUADRADO (X²) ENTRE IDADE E POPRUA_1/ CHI-SQUARE AGE:

quiqua_idade_categ_poprua <-chisq.test(tabela_idade_categ_poprua)
quiqua_idade_categ_poprua

chisq.test(tabela_idade_categ_poprua)$expected





#-----------------------#
#DESCRITIVA ESCOLARIDADE#
#-----------------------#

#BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#
#ABSOLUTA e RELATIVA
table(banco_9$escolaridade_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")
(table(banco_9$escolaridade_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$escolaridade_1[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#
#ABSOLUTA e RELATIVA
table(banco_9$escolaridade_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")
(table(banco_9$escolaridade_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$escolaridade_1[banco_9$pop_rua_1=="g.2 Sim"]))*100






#------------------------#
#DESCRITIVA ESCOLARIDADE2#
#------------------------#
#BIVARIADA

#---------------------#
#frequencias pop n-rua#
#---------------------#
#ABSOLUTA e RELATIVA
table(banco_9$escolaridade2[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")
(table(banco_9$escolaridade2[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$escolaridade2[banco_9$pop_rua_1=="g.1 Não"]))*100


#---------------------#
#frequencias pop rua  #
#---------------------#
#ABSOLUTA e RELATIVA
table(banco_9$escolaridade2[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")
(table(banco_9$escolaridade2[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$escolaridade2[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TESTE DE HIPOTESE with POP_RUA/ HIPOTHESIS TEST WITH POP_RUA
#TABELA DE CONTINGENCIA DE RACA E POPRUA_1/ CONTINGENCY TABLE:

tabela_escolar2_poprua <-table(banco_9$escolaridade2,banco_9$pop_rua_1)
tabela_escolar2_poprua

#TESTE QUI-QUADRADO (X²) ENTRE RACA E POPRUA_1/ CHI-SQUARE RACE:

quiqua_escolar2_poprua <-chisq.test(tabela_escolar2_poprua)
quiqua_escolar2_poprua

chisq.test(tabela_escolar2_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_escolar2_poprua



#------------------------------#
#DESCRITIVA BENEF. TRANS. RENDA#
#------------------------------#


#BIVARIADA

#---------------------#
#frequencias pop n-rua#
#---------------------#
table(banco_9$benefic_trans_renda[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$benefic_trans_renda[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$benefic_trans_renda[banco_9$pop_rua_1=="g.1 Não"]))*100


#---------------------#
#frequencias pop rua  #
#---------------------#
table(banco_9$benefic_trans_renda[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$benefic_trans_renda[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$benefic_trans_renda[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TESTE DE HIPOTESE with POP_RUA/ HIPOTHESIS TEST WITH POP_RUA
#TABELA DE CONTINGENCIA DE BENEFICIO E POPRUA_1/ CONTINGENCY TABLE:

tabela_benefic_trans_renda_poprua <-table(banco_9$benefic_trans_renda,banco_9$pop_rua_1)
tabela_benefic_trans_renda_poprua

#TESTE QUI-QUADRADO (X²) ENTRE FORMA DA TB E POPRUA_1/ CHI-SQUARE:

quiqua_benefic_trans_renda_poprua <-chisq.test(tabela_benefic_trans_renda_poprua)
quiqua_benefic_trans_renda_poprua

chisq.test(tabela_benefic_trans_renda_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_benefic_trans_renda_poprua



#----------------------#
#DESCRITIVA FORMA DA TB#
#----------------------#

#UNIVARIADA

table(banco_9$forma_clinica2,useNA = "always")
(table(banco_9$forma_clinica2,useNA = "always")/length(banco_9$forma_clinica2))*100


#BIVARIADA

#---------------------#
#frequencias pop n-rua#
#---------------------#
table(banco_9$forma_clinica2[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$forma_clinica2[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$forma_clinica2[banco_9$pop_rua_1=="g.1 Não"]))*100


#---------------------#
#frequencias pop rua  #
#---------------------#
table(banco_9$forma_clinica2[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$forma_clinica2[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$forma_clinica2[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TESTE DE HIPOTESE with POP_RUA/ HIPOTHESIS TEST WITH POP_RUA
#TABELA DE CONTINGENCIA DE FORMA DA TB E POPRUA_1/ CONTINGENCY TABLE:

tabela_formaTB_poprua <-table(banco_9$forma_clinica2,banco_9$pop_rua_1)
tabela_formaTB_poprua

#TESTE QUI-QUADRADO (X²) ENTRE FORMA DA TB E POPRUA_1/ CHI-SQUARE:

quiqua_formaTB_poprua <-chisq.test(tabela_formaTB_poprua)
quiqua_formaTB_poprua

chisq.test(tabela_formaTB_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_formaTB_poprua




#-------------------------------#
#DESCRITIVA ESTADOS DO BRASIL  #
#-----------------------------#
table(banco_9$estados,useNA = "always")
(table(banco_9$estados,useNA = "always")/length(banco_9$estados))*100


#---------------------#
#frequencias pop n-rua#
#---------------------#
table(banco_9$estados[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$estados[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$estados[banco_9$pop_rua_1=="g.1 Não"]))*100


#---------------------#
#frequencias pop rua  #
#---------------------#
table(banco_9$estados[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$estados[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$estados[banco_9$pop_rua_1=="g.2 Sim"]))*100





#---------------------------------#
#DESCRITIVA DAS CAPITAIS DO BRASIL#
#---------------------------------#

table(banco_9$capitais,useNA = "always")
unique(banco_9$capitais)


#existem valores missings na variavel, então vou filtra as capitais, sem NA
#there are NA in the variable, so i'll filter only the capitals, without NA



#teste_capitais<-banco_9%>%
 # filter(capitais=="g.1Rio Branco"|capitais=="g2.Maceio"|capitais=="g3.Macapá"|
           capitais=="g4.Manaus"|capitais=="g5.Salvador"|capitais=="g6.Fortaleza"|
           capitais=="g.7Brasilia"|capitais=="g8.Vitoria"|capitais=="g9.Goiania"|
           capitais=="g10.Sao Luis"|capitais=="g11.Cuiaba"|capitais=="g12.Campo Grande"|
           capitais=="g13.Belo Horizonte"|capitais=="g14.Belem"|capitais=="g15.Joao Pessoa"|
           capitais=="g16.Curitiba"|capitais=="g17.Recife"|capitais=="g18.Teresina"|
           capitais=="g19.Natal"|capitais=="g20.Porto Alegre"|capitais=="g21.Rio de Janeiro"|
           capitais=="g22.Porto Velho"|capitais=="g23.Boa Vista"|capitais=="g.24Floripa"|
           capitais=="g25.São Paulo"|capitais=="g26.Aracaju"|capitais=="g.27Palmas")


#table(teste_capitais$capitais,useNA = "always")

#---------------------#
#frequencias capitais #
#---------------------#

#table(teste_capitais$capitais,useNA = "always")
#(table(teste_capitais$capitais, useNA = "always")/length(teste_capitais$capitais))*100


#---------------------#
#frequencias pop n-rua#
#---------------------#
#table(teste_capitais$capitais[teste_capitais$pop_rua_1=="g.1 Não"], useNA = "always")
#(table(teste_capitais$capitais[teste_capitais$pop_rua_1=="g.1 Não"],useNA = "always")/length(teste_capitais$capitais[teste_capitais$pop_rua_1=="g.1 Não"]))*100


#---------------------#
#frequencias pop rua  #
#---------------------#
#table(teste_capitais$capitais[teste_capitais$pop_rua_1=="g.2 Sim"], useNA = "always")
#(table(teste_capitais$capitais[teste_capitais$pop_rua_1=="g.2 Sim"],useNA = "always")/length(teste_capitais$capitais[teste_capitais$pop_rua_1=="g.2 Sim"]))*100






#--------------------------------#
#DESCRITIVA DAS REGIOES DO BRASIL#
#--------------------------------#

table(banco_9$regioes_BR, useNA = "always")
(table(banco_9$regioes_BR,useNA = "always")/length(banco_9$regioes_BR))*100


#BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#
table(banco_9$regioes_BR[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$regioes_BR[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$regioes_BR[banco_9$pop_rua_1=="g.1 Não"]))*100




#---------------------#
#frequencias pop rua  #
#---------------------#
table(banco_9$regioes_BR[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$regioes_BR[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$regioes_BR[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TESTE DE HIPOTESE with POP_RUA/ HIPOTHESIS TEST WITH POP_RUA
#TABELA DE CONTINGENCIA DE REGIOES E POPRUA_1/ CONTINGENCY TABLE REGIONS:

tabela_regioes_BR_poprua <-table(banco_9$regioes_BR,banco_9$pop_rua_1)
tabela_regioes_BR_poprua

#TESTE QUI-QUADRADO (X²) ENTRE REGIOES E POPRUA_1/ CHI-SQUARE REGIONS:

quiqua_regioes_BR_poprua <-chisq.test(tabela_regioes_BR_poprua)
quiqua_regioes_BR_poprua

chisq.test(tabela_regioes_BR_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_regioes_BR_poprua






#-----------------#
#DESCRITIVA   HIV #
#-----------------#

#DESCRITIVA UNIVARIADA
table(banco_9$HIV_1,useNA = "always")
(table(banco_9$HIV_1,useNA = "always")/length(banco_9$HIV_1))*100


#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#
table(banco_9$HIV_1[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$HIV_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$HIV_1[banco_9$pop_rua_1=="g.1 Não"]))*100


#---------------------#
#frequencias pop rua  #
#---------------------#
table(banco_9$HIV_1[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$HIV_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$HIV_1[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TABELA DE CONTINGENCIA DE HIV E POPRUA_1/ CONTINGENCY TABLE HIV:

tabela_HIV_1_poprua <-table(banco_9$HIV_1,banco_9$pop_rua_1)
tabela_HIV_1_poprua

#TESTE QUI-QUADRADO (X²) ENTRE HIV E POPRUA_1/ CHI-SQUARE HIV:

quiqua_HIV_1_poprua <-chisq.test(tabela_HIV_1_poprua)
quiqua_HIV_1_poprua

chisq.test(tabela_HIV_1_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_HIV_1_poprua




#-----------------#
#DESCRITIVA ALCOOL#
#-----------------#

#DESCRITIVA UNIVARIADA
table(banco_9$ALCOOL_1,useNA = "always")
(table(banco_9$ALCOOL_1,useNA = "always")/length(banco_9$ALCOOL_1))*100



#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$ALCOOL_1[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$ALCOOL_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$ALCOOL_1[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$ALCOOL_1[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$ALCOOL_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$ALCOOL_1[banco_9$pop_rua_1=="g.2 Sim"]))*100




#TABELA DE CONTINGENCIA DE ALCOOL E POPRUA_1/ CONTINGENCY TABLE ALCHOL:

tabela_ALCOOL_1_poprua <-table(banco_9$ALCOOL_1,banco_9$pop_rua_1)
tabela_ALCOOL_1_poprua

#TESTE QUI-QUADRADO (X²) ENTRE ALCOOL E POPRUA_1/ CHI-SQUARE ALCHOL:

quiqua_ALCOOL_1_poprua <-chisq.test(tabela_ALCOOL_1_poprua)
quiqua_ALCOOL_1_poprua

chisq.test(tabela_ALCOOL_1_poprua)$expected





#-------------------#
#DESCRITIVA DIABETES#
#-------------------#

#DESCRITIVA UNIVARIADA
table(banco_9$DIABETES,useNA = "always")
(table(banco_9$DIABETES,useNA = "always")/length(banco_9$DIABETES))*100


#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$DIABETES[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$DIABETES[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$DIABETES[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$DIABETES[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$DIABETES[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$DIABETES[banco_9$pop_rua_1=="g.2 Sim"]))*100


#TABELA DE CONTINGENCIA DE DIABETES E POPRUA_1/ CONTINGENCY TABLE DIABETES:

tabela_DIABETES_poprua <-table(banco_9$DIABETES,banco_9$pop_rua_1)
tabela_DIABETES_poprua

#TESTE QUI-QUADRADO (X²) ENTRE DIABETES E POPRUA_1/ CHI-SQUARE DIABETES:

quiqua_DIABETES_poprua <-chisq.test(tabela_DIABETES_poprua)
quiqua_DIABETES_poprua

chisq.test(tabela_DIABETES_poprua)$expected

#retirar notacao cientifica
options(scipen = 999)
quiqua_DIABETES_poprua






#------------------------#
#DESCRITIVA DOENCA MENTAL#
#------------------------#

#DESCRITIVA UNIVARIADA
table(banco_9$Doenca_mental,useNA = "always")
(table(banco_9$Doenca_mental,useNA = "always")/length(banco_9$Doenca_mental))*100


#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$Doenca_mental[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$Doenca_mental[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$Doenca_mental[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$Doenca_mental[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$Doenca_mental[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$Doenca_mental[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TABELA DE CONTINGENCIA DE Doenca_mental E POPRUA_1/ CONTINGENCY TABLE mental disease:

tabela_Doenca_mental_poprua <-table(banco_9$Doenca_mental,banco_9$pop_rua_1)
tabela_Doenca_mental_poprua

#TESTE QUI-QUADRADO (X²) ENTRE DIABETES E POPRUA_1/ CHI-SQUARE DIABETES:

quiqua_Doenca_mental_poprua <-chisq.test(tabela_Doenca_mental_poprua)
quiqua_Doenca_mental_poprua

chisq.test(tabela_Doenca_mental_poprua)$expected




#------------------------#
#DESCRITIVA TABACO       #
#------------------------#
#DESCRITIVA UNIVARIADA
table(banco_9$TABACO_1,useNA = "always")
(table(banco_9$TABACO_1,useNA = "always")/length(banco_9$TABACO_1))*100


#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$TABACO_1[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$TABACO_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$TABACO_1[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$TABACO_1[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$TABACO_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$TABACO_1[banco_9$pop_rua_1=="g.2 Sim"]))*100


#TABELA DE CONTINGENCIA DE TABACO E POPRUA_1/ CONTINGENCY TABLE TABACO:

tabela_TABACO_1_poprua <-table(banco_9$TABACO_1,banco_9$pop_rua_1)
tabela_TABACO_1_poprua

#TESTE QUI-QUADRADO (X²) ENTRE TABACO E POPRUA_1/ CHI-SQUARE TABACO:

quiqua_TABACO_1_poprua <-chisq.test(tabela_TABACO_1_poprua)
quiqua_TABACO_1_poprua

chisq.test(tabela_TABACO_1_poprua)$expected




#------------------------#
#DESCRITIVA DROGA ILICITA#
#------------------------#

#DESCRITIVA UNIVARIADA
table(banco_9$DROGA_ILICITA,useNA = "always")
(table(banco_9$DROGA_ILICITA,useNA = "always")/length(banco_9$DROGA_ILICITA))*100

#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$DROGA_ILICITA[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$DROGA_ILICITA[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$DROGA_ILICITA[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$DROGA_ILICITA[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$DROGA_ILICITA[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$DROGA_ILICITA[banco_9$pop_rua_1=="g.2 Sim"]))*100


#TABELA DE CONTINGENCIA DE DROGA ILICIA E POPRUA_1/ CONTINGENCY TABLE ILICIT DROGUE:

tabela_DROGA_ILICITA_poprua <-table(banco_9$DROGA_ILICITA,banco_9$pop_rua_1)
tabela_DROGA_ILICITA_poprua

#TESTE QUI-QUADRADO (X²) ENTRE DROGA ILICITA E POPRUA_1/ CHI-SQUARE ILICIT DROGUE:

quiqua_DROGA_ILICITA_poprua <-chisq.test(tabela_DROGA_ILICITA_poprua)
quiqua_DROGA_ILICITA_poprua

chisq.test(tabela_DROGA_ILICITA_poprua)$expected



#------------------------#
#DESCRITIVA TDO          #
#------------------------#

#DESCRITIVA UNIVARIADA
table(banco_9$TDO_1,useNA = "always")
(table(banco_9$TDO_1,useNA = "always")/length(banco_9$TDO_1))*100

#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$TDO_1[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$TDO_1[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$TDO_1[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$TDO_1[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$TDO_1[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$TDO_1[banco_9$pop_rua_1=="g.2 Sim"]))*100


#TABELA DE CONTINGENCIA DE TDO E POPRUA_1/ CONTINGENCY TABLE DOT:

tabela_TDO_1_poprua <-table(banco_9$TDO_1,banco_9$pop_rua_1)
tabela_TDO_1_poprua

#TESTE QUI-QUADRADO (X²) ENTRE TDO E POPRUA_1/ CHI-SQUARE DOT:

quiqua_TDO_1_poprua <-chisq.test(tabela_TDO_1_poprua)
quiqua_TDO_1_poprua

chisq.test(tabela_TDO_1_poprua)$expected






#------------------------#
#DESCRITIVA DESFECHOS    #
#------------------------#

#DESCRITIVA UNIVARIADA

table(banco_9$desfecho2, useNA = "always")
(table(banco_9$desfecho2, useNA = "always")/length(banco_9$desfecho2))*100

#DESCRITIVA BIVARIADA
#---------------------#
#frequencias pop n-rua#
#---------------------#

table(banco_9$desfecho2[banco_9$pop_rua_1=="g.1 Não"], useNA = "always")
(table(banco_9$desfecho2[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$desfecho2[banco_9$pop_rua_1=="g.1 Não"]))*100

#---------------------#
#frequencias pop rua  #
#---------------------#

table(banco_9$desfecho2[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always")
(table(banco_9$desfecho2[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$desfecho2[banco_9$pop_rua_1=="g.2 Sim"]))*100



#TABELA DE CONTINGENCIA DE IDADE E POPRUA_1/ CONTINGENCY TABLE AGE:

tabela_desfecho2_poprua <-table(banco_9$desfecho2,banco_9$pop_rua_1)
tabela_desfecho2_poprua

#TESTE QUI-QUADRADO (X²) ENTRE IDADE E POPRUA_1/ CHI-SQUARE AGE:

quiqua_desfecho2_poprua <-chisq.test(tabela_desfecho2_poprua)
quiqua_desfecho2_poprua

chisq.test(tabela_desfecho2_poprua)$expected


#aplicando teste de Fisher /applying fisher's test

fisher_desfecho <- fisher.test(tabela_desfecho2_poprua)
fisher_desfecho

fisher_desfecho$p.value

fisher.test(table(banco_9$desfecho2,banco_9$pop_rua_1))

fisher.test(tabela_desfecho2_poprua,simulate.p.value=TRUE,B=1e7)



#------------------------------------#
#                                    #
#      Análise de sobrevida          #  
#        Survival analysis           #
#                                    #
#------------------------------------#

setwd("C:\\Users\\osinu\\OneDrive\\Área de Trabalho\\Pós-graduação\\Mestrado\\Dissertação\\Analises_dissertacao")

require(foreign)
require(tidyverse)
library(dplyr)


banco_9 <-read.csv("dissertacao_survival_1.csv")

#gerando desfecho dicotomico sucesso e insucesso
#generating dichotomous outcome success and unsuccess

banco_9<-banco_9%>% 
  mutate(sucesso_insucesso =case_when(desfecho2=="g1.sucsess"~"g.2 success",
                                      (desfecho2=="g2.loss_follow_up")|(desfecho2=="g3.death_geral")|(desfecho2=="g4.failure")|(desfecho2=="g5.Not_evaluated")~"g.1 insuccess"))
                                      

table(banco_9$sucesso_insucesso, useNA = "always") 
(table(banco_9$sucesso_insucesso, useNA = "always")/length(banco_9$sucesso_insucesso))*100



#TABELA 2- DESCRITIVA COM AS POPULAÇÕES E O DESFECHO
#TABLE 2 - DESCRIPTIVE OF POPULATIONS AND OUTCOME

table(banco_9$pop_rua_1,useNA = "always")
(table(banco_9$pop_rua_1,useNA = "always")/length(banco_9$pop_rua_1))*100

#POP_RUA
#HOMELESS
table(banco_9$sucesso_insucesso[banco_9$pop_rua_1=="g.2 Sim"], useNA = "always") 
(table(banco_9$sucesso_insucesso[banco_9$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_9$sucesso_insucesso[banco_9$pop_rua_1=="g.2 Sim"]))*100


#NAO RUA
#NOT HOMELESS
table(banco_9$sucesso_insucesso[banco_9$pop_rua_1=="g.1 Não"], useNA = "always") 
(table(banco_9$sucesso_insucesso[banco_9$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_9$sucesso_insucesso[banco_9$pop_rua_1=="g.1 Não"]))*100


#confirmando se ha missing nas variaveis
#confirming if there are missings values in the variables

any(is.na(banco_9$sucesso_insucesso)) #comando que mostra se ha ou não NA                                    
any(is.na(banco_9$pop_rua_1)) #comando que mostra se ha ou não NA



#TESTE DE HIPOTESE c DESFECHO DICOTOMICO
#HIPOTHESIS TEST WITH DICHOTOMOUS OUTCOME

#TABELA DE CONTINGENCIA DE INSUCESSO E POPRUA_1:

tabela_insuc_poprua <-table(banco_9$sucesso_insucesso,banco_9$pop_rua_1)
tabela_insuc_poprua

#TESTE QUI-QUADRADO (X²) ENTRE INSUCESSO E POPRUA_1:

quiqua_insu_poprua <-chisq.test(tabela_insuc_poprua)
quiqua_insu_poprua

#retirar notacao cientifica
options(scipen = 999)
quiqua_insu_poprua

chisq.test(tabela_insuc_poprua)$expected



#------------------------------------#
#NOVA VARIABLE DESFECHO VALORES 0,1  #
#NEW OUTCOME VARIABLE WITH VALEUS 0,1#
#------------------------------------#

unique(banco_9$sucesso_insucesso)

banco_9<-banco_9%>% 
  mutate(sucesso_insucesso_2=case_when(sucesso_insucesso=="g.2 success"~0,
                                       (sucesso_insucesso=="g.1 insuccess"~1)))

table(banco_9$sucesso_insucesso_2, useNA = "always")
table(banco_9$benefic_trans_renda, useNA = "always")
any(is.na(banco_9$sucesso_insucesso_2)) #comando que mostra se ha ou g.1 Não NA                                    

banco_10<-banco_9%>%select(sexo1,
                            raca_2,
                            idade_categ,
                            HIV_1,
                            regioes_BR,
                            benefic_trans_renda,
                            forma_clinica2,
                            ALCOOL_1,
                            DIABETES,
                            Doenca_mental,
                            TABACO_1,
                            DROGA_ILICITA,
                            pop_rua_1,
                            escolaridade2,
                            TDO_1,
                            sucesso_insucesso,
                            sucesso_insucesso_2,
                            dif_encerr_inic,
                            desfecho1,
                            desfecho2)


banco_11 <-banco_10%>%
  filter(escolaridade2=="g1.Ens.Superior_ou_mais"|escolaridade2=="g2.Ens.Medio"|escolaridade2=="g3.Ens.Fund.I-II"|escolaridade2=="g4.analfabetos")%>%
  filter(TDO_1=="g.1Sim"|TDO_1=="g.2Não")%>%
  filter(HIV_1=="g.1 Ñ_coinfecção"|HIV_1=="g.2 Coinfecção")%>%
  filter(raca_2=="g.1Brancos"|raca_2=="g.2Negros"|raca_2=="g.3Amarelos"|raca_2=="g.4indígena")
  

#POP_RUA
#HOMELESS
table(banco_11$pop_rua_1, useNA = "always") 
(table(banco_11$pop_rua_1,useNA = "always")/length(banco_11$pop_rua_1))*100





 
 

#------------------------#
#  Modelagem/Modeling    #
#------------------------#

library("survival")
library("survminer") 

#PARA EVITAR QUE O SCRIPT FIQUE LONGO, EU MUDEI A VARIAVEL APÓS O ~
#TO AVOID THAT THE SCRIPT BECOMES TO LONGO, I CHANGED VARIABLE AFTER ~

#SOBREVIDA BRUTA-Desfecho 
#CRUDE SURVIVAL- OUTCOME


#crude model/ modelagem bruta


fit_bruto <- coxph(Surv(dif_encerr_inic, sucesso_insucesso_2)~ raca_2, data=banco_11)
fit_bruto


# HR,IC, p-valor e logrank
summary(fit_bruto)





#SOBREVIDA AJUSTADA -DESFECHO
#ADJUSTED SURVIVAL- OUTCOME


fit_adj <- coxph(Surv(dif_encerr_inic, sucesso_insucesso_2)~pop_rua_1 + sexo1 + raca_2 +
                    idade_categ + escolaridade2 + forma_clinica2 + HIV_1 + ALCOOL_1 + 
                    DIABETES + Doenca_mental + TABACO_1 + DROGA_ILICITA + TDO_1, data=banco_11)

fit_adj 

# HR,IC, p-valor e logrank
summary(fit_adj)


#DESCRITIVA DE CADA VARIAVEL
#DESCRIPTIVE FOR EACH VARIABLE
table(banco_11$TDO_1[banco_11$pop_rua_1=="g.2 Sim"],useNA = "always")
(table(banco_11$TABACO_1[banco_11$pop_rua_1=="g.2 Sim"],useNA = "always")/length(banco_11$TABACO_1[banco_11$pop_rua_1=="g.2 Sim"]))*100



table(banco_11$forma_clinica2[banco_11$pop_rua_1=="g.1 Não"],useNA = "always")
(table(banco_11$TABACO_1[banco_11$pop_rua_1=="g.1 Não"],useNA = "always")/length(banco_11$TABACO_1[banco_11$pop_rua_1=="g.1 Não"]))*100




#-------------------------------------#
# PRESSUPOSTO DOS RISCOS PROPORCIONAIS#
# PROPORTIONAL HAZARD ASSUMPTIONS     #
#-------------------------------------#

#----------------------------#
#Análise de resíduos- POP RUA#
#----------------------------#

resid_rua <-coxph(Surv(dif_encerr_inic,sucesso_insucesso_2)~pop_rua_1 + sexo1 + raca_2 +
                    idade_categ + escolaridade2 + forma_clinica2 + HIV_1 + ALCOOL_1 + 
                    DIABETES + Doenca_mental + TABACO_1 + DROGA_ILICITA + TDO_1, data=banco_11)
resid_rua


#-------------------------------------#
# proportional-hazards (PH) assumption#
#-------------------------------------#
test.ph.rua <- cox.zph(resid_rua)
test.ph.rua



#--------------------------#
#Schoenfeld individual test#
#--------------------------#

ggcoxzph(test.ph.rua)





#Curva de Kaplan-meier
#Kaplan-meier curve

sfit_exposicao<- coxph(Surv(dif_encerr_inic, sucesso_insucesso_2)~strata(pop_rua_1) + sexo1 + raca_2 +
                         idade_categ + escolaridade2 + forma_clinica2 + HIV_1 + ALCOOL_1 +
                         DIABETES + Doenca_mental + TABACO_1 + DROGA_ILICITA + TDO_1, data=banco_11)


summary(sfit_exposicao)


sfit_2 <-survfit(sfit_exposicao)

str(sfit_2)

km_expo <-data.frame(time=sfit_2$time,
                     surv=sfit_2$surv,
                     surv2=1-sfit_2$surv,
                     strata=rep(names(sfit_2$strata),sfit_2$strata))


head(km_expo)
table(km_expo$strata)

km_expo<-km_expo%>%mutate(strata=case_when(strata=="g.1 Não" ~ "Sheltered",
                                           strata=="g.2 Sim" ~ "Homeless"))


ggplot(km_expo,aes(x=time, y=surv, color=strata))+
  geom_line()+
  labs(
    title = "Unsuccessful tuberculosis treatment in six months",
    subtitle = ,
    x = "Time (270 days)",
    y = "Survival rate",
    caption = "Source: SINAN,August 2021"
  )+
  theme(legend.title=element_blank())


#Gráfico 
ggplot(km_expo,aes(x=time, y=surv2, color=strata))+
  geom_line()+
  labs(
    title = "Unsuccessful tuberculosis treatment",
    subtitle = ,
    x = "Time (270 days)",
    y = "Cumulative incidence",
    caption = "Source: SINAN,August 2021"
  )+
  theme(legend.title=element_blank())





#fit <- survfit(Surv(dif_encerr_inic, sucesso_insucesso_2) ~ pop_rua_1, data = banco_11)
#plot(fit) # the standard survival curve
#plot(fit, fun="event") # the cumulative incidence curve




#--------------------------------------#
#FATORES ASSOCIADOS /FACTORS ASSOCIATED#
#--------------------------------------#

#alternativa 1
SA_rua <-banco_11 %>%
  filter(pop_rua_1=="g.2 Sim")%>%
  filter(raca_2=="g.1Brancos"|raca_2=="g.2Negros")


#alternativa 2
#Modelagem Cox/Modeling COX

table(SA_rua$pop_rua_1, useNA = "always")
table(SA_rua$pop_rua_1)


#univariate model/modelagem univariada
#just changed the covariables/apenas mudei as covariaveis


fit_fator_bruto <- coxph(Surv(dif_encerr_inic, sucesso_insucesso_2)~ raca_2 ,data=SA_rua)

fit_fator_bruto


# HR,IC, p-valor e logrank
summary(fit_fator_bruto)




#modelo ajustado / adjusted model
fit_fator_rua <- coxph(Surv(dif_encerr_inic, sucesso_insucesso_2)~ sexo1 + 
                         raca_2 + idade_categ + escolaridade2 + forma_clinica2 + HIV_1 +
                         ALCOOL_1 + DIABETES + Doenca_mental + TABACO_1 + DROGA_ILICITA + TDO_1,
                       data=SA_rua)

fit_fator_rua 

# HR,IC, p-valor e logrank
summary(fit_fator_rua )



#-------------------------------------#
# PRESSUPOSTO DOS RISCOS PROPORCIONAIS#
# PROPORTIONAL HAZARD ASSUMPTIONS     #
#-------------------------------------#

#----------------------------#
#Análise de resíduos- POP RUA#
#----------------------------#

resid_rua_2 <-coxph(Surv(dif_encerr_inic,sucesso_insucesso_2)~sexo1 + 
                      raca_2 + idade_categ + escolaridade2 + forma_clinica2 + HIV_1 +
                      ALCOOL_1 + DIABETES + Doenca_mental + TABACO_1 + DROGA_ILICITA + TDO_1,
                    data=SA_rua)
resid_rua_2


#-------------------------------------#
# proportional-hazards (PH) assumption#
#-------------------------------------#
test.ph.rua.2 <- cox.zph(resid_rua_2)
test.ph.rua.2



#--------------------------#
#Schoenfeld individual test#
#--------------------------#

ggcoxzph(test.ph.rua.2)






#-------------------------------------------#
#analizando cada categoria do desfecho final#
#-------------------------------------------#

#-----------------#
#COX PARA ABANDONO#
#-----------------#

SA_rua<-SA_rua%>% 
  mutate(abandoned=ifelse((desfecho2=="g2.loss_follow_up"),1,0))


table(SA_rua$abandoned, useNA = "always") 


fit_crude_abandoned <- coxph(Surv(dif_encerr_inic, abandoned)~ TDO_1 ,data=SA_rua)

fit_crude_abandoned


# HR,IC, p-valor e logrank
summary(fit_crude_abandoned)




fit_abandoned <- coxph(Surv(dif_encerr_inic, abandoned)~ DROGA_ILICITA + Doenca_mental +TDO_1 ,data=SA_rua)

fit_abandoned


# HR,IC, p-valor e logrank
summary(fit_abandoned)





#-----------------#
#COX PARA MORTES  #
#-----------------#
#nova variavel mortes
SA_rua<-SA_rua%>% 
  mutate(alldeaths=ifelse((desfecho2=="g3.death_geral"),1,0))


table(SA_rua$alldeaths, useNA = "always") 


fit_crude_alldeaths <- coxph(Surv(dif_encerr_inic, alldeaths)~ TDO_1,data=SA_rua)

fit_crude_alldeaths


# HR,IC, p-valor e logrank
summary(fit_crude_alldeaths)



fit_alldeaths <- coxph(Surv(dif_encerr_inic, alldeaths)~ DROGA_ILICITA + Doenca_mental +TDO_1 ,data=SA_rua)

fit_alldeaths


# HR,IC, p-valor e logrank
summary(fit_alldeaths)





#-----------------#
#COX PARA FALENCIA#
#-----------------#
#nova variavel falencia
SA_rua<-SA_rua%>% 
  mutate(failure=ifelse((desfecho2=="g4.failure"),1,0))


table(SA_rua$failure, useNA = "always") 

fit_failure <-coxph(Surv(dif_encerr_inic, failure)~ DROGA_ILICITA + Doenca_mental +TDO_1 ,data=SA_rua)

fit_failure


# HR,IC, p-valor e logrank
summary(fit_failure)





#---------------------#
#COX PARA N_AVALIADO  #
#---------------------#
#nova variavel mortes
SA_rua<-SA_rua%>% 
  mutate(not_evaluated=ifelse((desfecho2=="g5.Not_evaluated"),1,0))


table(SA_rua$not_evaluated, useNA = "always") 


fit_notevalut <- coxph(Surv(dif_encerr_inic, not_evaluated)~ DROGA_ILICITA + Doenca_mental +TDO_1 ,data=SA_rua)

fit_notevalut


# HR,IC, p-valor e logrank
summary(fit_notevalut)


























#------------------------#
#  Modelagem/Modeling    #
# without ignored variab.#
#------------------------#


banco_11 <-banco_10%>%
  filter(sexo1=="g.1 Feminino"|sexo1=="g.2 Masculino",
         raca_2=="g.1Brancos"|raca_2=="g.2Negros"|raca_2=="g.3Amarelos"|raca_2=="g.4indígena",
         idade_categ=="g1.18a29"|idade_categ=="g2.30a39"|idade_categ=="g3.40a49"|idade_categ=="g4.50a59"|idade_categ=="g5.60a64"|idade_categ=="g6.65a90",
         regioes_BR=="g.1 Centro_Oeste"|regioes_BR=="g.2 Norte"|regioes_BR=="g.3 Nordeste"|regioes_BR=="g.4 Sudestes"|regioes_BR=="g.5 Sul",
         HIV_1=="g.1 Ñ_coinfecção"|HIV_1=="g.2 Coinfecção",
         forma_clinica2=="g1.extrapulmonar"|forma_clinica2=="g2.Pulmonar",
         ALCOOL_1=="g.1 Não"|ALCOOL_1=="g.2 Sim",
         DIABETES=="g.1 Não"|DIABETES=="g.2 Sim",
         Doenca_mental=="g.1 Não"|Doenca_mental=="g.2 Sim",
         TABACO_1=="g.1Não"|TABACO_1=="g.2Sim",
         DROGA_ILICITA=="g.1Não"|DROGA_ILICITA=="g.2Sim",
         pop_rua_1=="g.1 Não"|pop_rua_1=="g.2 Sim",
         escolaridade2=="g1.Ens.Superior_ou_mais"|escolaridade2=="g2.Ens.Medio"|escolaridade2=="g3.Ens.Fund.I-II"|escolaridade2=="g4.analfabetos",
         TDO_1=="g.1Sim"|TDO_1=="g.2Não",
         sucesso_insucesso=="g.1 insuccess"|sucesso_insucesso=="g.2 success")



