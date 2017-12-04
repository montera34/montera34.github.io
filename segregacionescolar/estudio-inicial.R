install.packages("MASS")
library("MASS")

segreg = read.csv("data/output/2014-15-variables-escolares-euskadi.csv")
attach(segreg)


parcoord(segreg[,c(4:20)])
parcoord(segreg[,c(4,5)])

parcoord(segreg[,c(4:20)],col=rainbow(length(segreg[,1])))

parcoord(segreg[,c(3,4,5)],col=rainbow(length(segreg[,1])))
parcoord(segreg[,c(4,5)],col=rainbow(length(segreg[,3])))

parcoord(cardeaths, col=rainbow(length(cardeaths[,1])), var.label=TRUE)

library("tidyverse")

plot(perc_alum_ext_pub, perc_alum_ext_priv, main="% extranjeros",
     xlab="red público", ylab="red privada",xlim = c(0,40),ylim = c(0,40),
     col=provincia
     ) 
text(perc_alum_ext_pub, perc_alum_ext_priv,zona,cex=0.6, pos=4, col="grey")
abline(a = 0,b = 1) #pendiente y = x

# Add fit lines
abline(lm(perc_alum_ext_pub~perc_alum_ext_priv,), col="red") # regression line (y~x)
lines(lowess(perc_alum_ext_pub,perc_alum_ext_priv), col="blue") # lowess line (x,y) 

ggplot(data = segreg, mapping = aes(x = perc_alum_ext_pub, y = perc_alum_ext_priv)) + 
  coord_cartesian(ylim = c(0, 40),xlim = c(0, 40)) +
  scale_colour_manual(values=c("#f6ae01", "#4199cb","#da5455")) +
  geom_abline(intercept=0,slope=1, colour="grey") +
  geom_point(aes(colour = provincia,size = total_alumnado)) +
  labs(title = "% alumnado extranjero") +
  geom_text(aes(label=zona),size = 3, colour="black", nudge_x = 0.7, nudge_y = 1,fill = provincia)

plot.y <- qplot(y=Temp, x=Wind, data=data)
model.y <- lm(Temp ~ Wind, data)
coef.y <- coef(model.y)
plot.y + geom_abline(intercept=coef.y[1],
                     slope=coef.y[2])

abline(a = 0,b = 1) #pendiente y = x

ggplot(data = segreg, mapping = aes(x = perc_alum_ext_pub)) +
  geom_histogram(binwidth = 4)





my.pairs <- function (x, labels, panel = points, ..., lower.panel = panel, 
                      upper.panel = panel, diag.panel = NULL, text.panel = textPanel, 
                      label.pos = 0.5 + has.diag/3, line.main = 3, cex.labels = NULL, 
                      font.labels = 1, row1attop = TRUE, gap = 1, log = "", xlim=NULL, ylim=NULL) 
{
  if (doText <- missing(text.panel) || is.function(text.panel)) 
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x, 
                                                                 y, txt, cex = cex, font = font)
  localAxis <- function(side, x, y, xpd, bg, col = NULL, main, 
                        oma, ...) {
    xpd <- NA
    if (side%%2L == 1L && xl[j]) 
      xpd <- FALSE
    if (side%%2L == 0L && yl[i]) 
      xpd <- FALSE
    if (side%%2L == 1L) 
      Axis(x, side = side, xpd = xpd, ...)
    else Axis(y, side = side, xpd = xpd, ...)
  }
  localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
  localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
  localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
  localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
  dots <- list(...)
  nmdots <- names(dots)
  if (!is.matrix(x)) {
    x <- as.data.frame(x)
    for (i in seq_along(names(x))) {
      if (is.factor(x[[i]]) || is.logical(x[[i]])) 
        x[[i]] <- as.numeric(x[[i]])
      if (!is.numeric(unclass(x[[i]]))) 
        stop("non-numeric argument to 'pairs'")
    }
  }
  else if (!is.numeric(x)) 
    stop("non-numeric argument to 'pairs'")
  panel <- match.fun(panel)
  if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
    lower.panel <- match.fun(lower.panel)
  if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
    upper.panel <- match.fun(upper.panel)
  if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel)) 
    diag.panel <- match.fun(diag.panel)
  if (row1attop) {
    tmp <- lower.panel
    lower.panel <- upper.panel
    upper.panel <- tmp
    tmp <- has.lower
    has.lower <- has.upper
    has.upper <- tmp
  }
  nc <- ncol(x)
  if (nc < 2) 
    stop("only one column in the argument to 'pairs'")
  if (doText) {
    if (missing(labels)) {
      labels <- colnames(x)
      if (is.null(labels)) 
        labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels)) 
      doText <- FALSE
  }
  oma <- if ("oma" %in% nmdots) 
    dots$oma
  main <- if ("main" %in% nmdots) 
    dots$main
  if (is.null(oma)) 
    oma <- c(4, 4, if (!is.null(main)) 6 else 4, 4)
  opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
  on.exit(par(opar))
  dev.hold()
  on.exit(dev.flush(), add = TRUE)
  xl <- yl <- logical(nc)
  if (is.numeric(log)) 
    xl[log] <- yl[log] <- TRUE
  else {
    xl[] <- grepl("x", log)
    yl[] <- grepl("y", log)
  }
  for (i in if (row1attop) 
    1L:nc
    else nc:1L) for (j in 1L:nc) {
      l <- paste0(ifelse(xl[j], "x", ""), ifelse(yl[i], "y", 
                                                 ""))
      if (is.null(xlim) & is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l)
      if (is.null(xlim) & !is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l, ylim=ylim[j,i,])
      if (!is.null(xlim) & is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l, xlim = xlim[j,i,])
      if (!is.null(xlim) & !is.null(ylim))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                  type = "n", ..., log = l, xlim = xlim[j,i,], ylim=ylim[j,i,])
      
      if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
        box()
        if (i == 1 && (!(j%%2L) || !has.upper || !has.lower)) 
          localAxis(1L + 2L * row1attop, x[, j], x[, i], 
                    ...)
        if (i == nc && (j%%2L || !has.upper || !has.lower)) 
          localAxis(3L - 2L * row1attop, x[, j], x[, i], 
                    ...)
        if (j == 1 && (!(i%%2L) || !has.upper || !has.lower)) 
          localAxis(2L, x[, j], x[, i], ...)
        if (j == nc && (i%%2L || !has.upper || !has.lower)) 
          localAxis(4L, x[, j], x[, i], ...)
        mfg <- par("mfg")
        if (i == j) {
          if (has.diag) 
            localDiagPanel(as.vector(x[, i]), ...)
          if (doText) {
            par(usr = c(0, 1, 0, 1))
            if (is.null(cex.labels)) {
              l.wid <- strwidth(labels, "user")
              cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
            }
            xlp <- if (xl[i]) 
              10^0.5
            else 0.5
            ylp <- if (yl[j]) 
              10^label.pos
            else label.pos
            text.panel(xlp, ylp, labels[i], cex = cex.labels, 
                       font = font.labels)
          }
        }
        else if (i < j) 
          localLowerPanel(as.vector(x[, j]), as.vector(x[, 
                                                         i]), ...)
        else localUpperPanel(as.vector(x[, j]), as.vector(x[, 
                                                            i]), ...)
        if (any(par("mfg") != mfg)) 
          stop("the 'panel' function made a new plot")
      }
      else par(new = FALSE)
    }
  if (!is.null(main)) {
    font.main <- if ("font.main" %in% nmdots) 
      dots$font.main
    else par("font.main")
    cex.main <- if ("cex.main" %in% nmdots) 
      dots$cex.main
    else par("cex.main")
    mtext(main, 3, line.main, outer = TRUE, at = 0.5, cex = cex.main, 
          font = font.main)
  }
  invisible(NULL)
}


my_line <- function(x,y){
  abline(a = 0,b = 1)
}

pairs(~perc_alum_ext_pub+perc_alum_ext_priv+perc_bec_comedor_pub+perc_bec_comedor_priv+perc_bec_mat_escolar_pub+perc_bec_mat_escolar_priv,data=segreg,
      xlim=c(0,70),ylim = c(0,70),
      lower.panel = abline(a = 0,b = 1))
      

pairs(iris[1:4],xlim=c(0,8), ylim = c(0,8)) 
