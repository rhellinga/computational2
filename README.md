---
title: "index.Rmd"
author: "Rens Hellinga"
date: "24 februari 2019"
output: 
  flexdashboard::flex_dashboard:
    storyboard: true
    
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(plotly)
library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = 'c620947a507546308cef348f88cb2354')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2eefd3dc94344ba389c0dfae7502f95f')
```

### Introduction to the Music of Miles Davis

Balance in the music of Miles Davis

Jazz musician Miles Davis has made music in a variety of styles, in which acoustic instruments as well as electric instruments play a big role. the main topic of my research will be how the acousticness stands in relation to other variables, like energy, in the music. For this I will be using the 'This is Miles Davis' list from Spotify because this represents the oeuvre of Miles Davis well. Beside of the overall list of Miles Davis' hits I will look closer to the details of a couple of leading albums (Relaxin' with the Miles Davis Quintet, Kind of Blue, Nefertiti, Bitches Brew, You're Under Arrest and Doo-bop) and compare the means of these albums to get a good view of how the music of Davis changes through the years (as he becomes Electric Miles). 

I have found out that if you compare the album Kind of Blue (1959) and Doo-Bop (1992) that while the mean of the energy on Kind of Blue is 0.165 the mean of the energy on Doo-Bop is 0.678. The means of the acousticness respectively are 0.715 and 0.00697.
Below you can see a graph in which the acousticness is set against the energy.

### Findings

```{r echo=FALSE}
MilesDavis <- get_playlist_audio_features('spotify', '37i9dQZF1DX3BqHI5fuXWV')
plotMD <-
     MilesDavis %>%                   # Start with awards.
     ggplot(                      # Set up the plot.
         aes(
             x = acousticness,
             y = energy,
             size = track_popularity,
             color = loudness,
             label = track_name,
         )
     ) +
     geom_point() +               # Scatter plot.
     geom_smooth(method="loess", se=TRUE, alpha = 0.3) +
     theme_bw()
 ggplotly(plotMD)
```

***

Acousticness vs Energy

In this graph you can see the acousticness of the music set against the energy If you move over the dots you can see which track is standing where. This is an important feature because now you can actually see which track of Davis it is, and thus in which period he made it. Knowing this you it is possible to conclude what kind of music Davis made in different periods of his life. 

### Audio Analysis

```{r echo=FALSE}
get_tidy_audio_analysis <- function(track_uri, ...)
{
    get_track_audio_analysis('1QxFPdYTJ1F5dTZwaJviSj') %>% 
        list %>% transpose %>% as_tibble %>% 
        mutate_at(vars(meta, track), . %>% map(as_tibble)) %>% 
        unnest(meta, track) %>% 
        select(
            analyzer_version,
            duration,
            contains('fade'),
            ends_with('confidence'),
            bars:segments) %>% 
        mutate_at(
            vars(bars, beats, tatums, sections), 
            . %>% map(bind_rows)) %>% 
        mutate(
            segments =
                map(
                    segments,
                    . %>% 
                        transpose %>% as_tibble %>% 
                        unnest(.preserve = c(pitches, timbre)) %>% 
                        mutate(
                            pitches = 
                                map(
                                    pitches, 
                                    . %>% 
                                        flatten_dbl %>% 
                                        set_names(
                                            c( 
                                                'C', 'C#|Db', 'D', 'D#|Eb', 
                                                'E', 'F', 'F#|Gb', 'G',
                                                'G#|Ab', 'A', 'A#|Bb', 'B'))),
                            timbre = 
                                map(
                                    timbre,
                                    . %>% 
                                        flatten_dbl %>% 
                                        set_names(
                                            c(
                                                'c1', 'c2', 'c3', 'c4', 
                                                'c5', 'c6', 'c7', 'c8',
                                                'c9', 'c10', 'c11', 'c12'))))))
}
```

```{r echo=FALSE} 
#' Normalise vectors for Computational Musicology.
#'
#' We use a number of normalisation strategies in Computational Musicology.
#' This function brings them together into one place, along with common
#' alternative names.
compmus_normalise <- compmus_normalize <- function(v, method = "euclidean")
{
    ## Supported functions
    
    harmonic  <- function(v) v * sum(1 / abs(v))
    manhattan <- function(v) v / sum(abs(v))
    euclidean <- function(v) v / sqrt(sum(v^2))
    chebyshev <- function(v) v / max(abs(v))
    clr       <- function(v) {lv <- log(v); lv - mean(lv)}
    
    ## Method aliases
    
    METHODS <-
        list(
            harmonic  = harmonic,
            manhattan = manhattan,
            L1        = manhattan,
            euclidean = euclidean,
            L2        = euclidean,
            chebyshev = chebyshev,
            maximum   = chebyshev,
            aitchison = clr,
            clr       = clr)
    
    ## Function selection
    
    if (!is.na(i <- pmatch(method, names(METHODS))))
        METHODS[[i]](v)
    else 
        stop('The method name is ambiguous or the method is unsupported.')
}
#' Compute pairwise distances for Computational Musicology in long format.
#'
#' We use a number of distance measures in Computational Musicology.
#' This function brings them together into one place, along with common
#' alternative names. It is designed for convenience, not speed.
compmus_long_distance <- function(xdat, ydat, feature, method = "euclidean")
{
    
    feature <- enquo(feature)
    
    ## Supported functions
    
    manhattan <- function(x, y) sum(abs(x - y))
    euclidean <- function(x, y) sqrt(sum((x - y) ^ 2))
    chebyshev <- function(x, y) max(abs(x - y))
    pearson   <- function(x, y) 1 - cor(x, y)
    cosine    <- function(x, y)
    {
        1 - sum(compmus_normalise(x, "euc") * compmus_normalise(y, "euc"))
    }
    angular   <- function(x, y) 2 * acos(1 - cosine(x, y)) / pi
    aitchison <- function(x, y)
    {
        euclidean(compmus_normalise(x, "clr"), compmus_normalise(y, "clr"))
    }
    
    ## Method aliases
    
    METHODS <-
        list(
            manhattan   = manhattan,
            cityblock   = manhattan,
            taxicab     = manhattan,
            L1          = manhattan,
            totvar      = manhattan,
            euclidean   = euclidean,
            L2          = euclidean,
            chebyshev   = chebyshev,
            maximum     = chebyshev,
            pearson     = pearson,
            correlation = pearson,
            cosine      = cosine,
            angular     = angular,
            aitchison   = aitchison)
    
    ## Function selection
    
    if (!is.na(i <- pmatch(method, names(METHODS))))
        bind_cols(
            crossing(
                xdat %>% select(xstart = start, xduration = duration),
                ydat %>% select(ystart = start, yduration = duration)),
            xdat %>% select(x = !!feature) %>% 
                crossing(ydat %>% select(y = !!feature)) %>% 
                transmute(d = map2_dbl(x, y, METHODS[[i]])))
    else 
        stop('The method name is ambiguous or the method is unsupported.')
}
```


```{r echo=FALSE}
#' Gathers chroma vectors into long format.
#'
#' Gathers chroma vectors into long format for Computational Musicology.
compmus_gather_chroma <- function(data)
{
    data %>% 
    mutate(pitches = map(pitches, bind_rows)) %>% unnest(pitches) %>% 
    gather("pitch_class", "value", C:B) %>% 
    mutate(pitch_class = fct_shift(factor(pitch_class), 3))
}
```

```{r echo=FALSE}
Blue_in_Green <- 
    get_tidy_audio_analysis('1QxFPdYTJ1F5dTZwaJviSj') %>% 
    select(segments) %>% unnest(segments) %>% 
    select(start, duration, pitches)
```

```{r echo=FALSE}
Blue_in_Green %>% 
    mutate(pitches = map(pitches, compmus_normalise, 'chebyshev')) %>% 
    compmus_gather_chroma %>% 
    ggplot(
        aes(
            x = start + duration / 2, 
            width = duration, 
            y = pitch_class, 
            fill = value)) + 
    geom_tile() +
    labs(x = 'Time (s)', y = NULL, fill = 'Magnitude') +
    theme_minimal()
```
