library(tidyverse)


lagoinha <- readxl::read_xls(path = "dados.xls",
                             sheet = 4)
taubate <- readxl::read_xls(path = "dados.xls",
                            sheet = 6)


lagoinha_scaled <- lagoinha %>% 
  mutate(Exp = scale(Exp)[,1],
         Meth = scale(Meth)[,1])

taubate_scaled <- taubate %>% 
  mutate(Exp = scale(Exp)[,1],
         Meth = scale(Meth)[,1])

shapiro.test(lagoinha$Exp)
shapiro.test(lagoinha$Meth)

shapiro.test(lagoinha_scaled$Exp)
shapiro.test(lagoinha_scaled$Meth)



shapiro.test(taubate$Exp)
shapiro.test(taubate$Meth)




cor(x = lagoinha_scaled$Exp,y = lagoinha_scaled$Meth,
    method = "spearman")

cor(x = taubate_scaled$Exp,y = taubate_scaled$Meth,
    method = "spearman")


ggplot(lagoinha_scaled, aes(x = Exp, y = Meth)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate(geom = "label",
           x = 1.5,
           y = 2,
           label = as.character(
             round(cor(x = lagoinha_scaled$Exp,y = lagoinha_scaled$Meth,
                                    method = "spearman"),4)
             )
           )
ggsave(filename = "lagoinha.png",
       dpi = 400,
       bg = "white",
       scale = 2)

ggplot(taubate_scaled, aes(x = Exp, y = Meth)) +
  geom_point() +
  geom_smooth(method = "lm")  +
  annotate(geom = "label", 
           x = 3,
           y = 3,
           label = as.character(
             round(cor(x = taubate_scaled$Exp,y = taubate_scaled$Meth,
                                    method = "spearman"),4)
             )
           )

ggsave(filename = "taubate.png",
       dpi = 400,
       bg = "white",
       scale = 2)
