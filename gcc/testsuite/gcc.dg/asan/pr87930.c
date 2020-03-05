/* { dg-do compile { target { { i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } && lp64 } } } */
/* { dg-options "-fsanitize=address -mabi=ms" } */

int i;

/* { dg-error ".-mabi=ms. not supported with .-fsanitize=address." "" { target *-*-* } 0 } */
