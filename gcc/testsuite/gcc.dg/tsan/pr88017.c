/* { dg-do compile { target { { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } && lp64 } } } */
/* { dg-options "-fsanitize=thread -mabi=ms" } */

int i;

/* { dg-error ".-mabi=ms. not supported with .-fsanitize=thread." "" { target *-*-* } 0 } */
