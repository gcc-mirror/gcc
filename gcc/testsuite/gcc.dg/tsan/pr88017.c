/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-fsanitize=thread -mabi=ms" } */

int i;

/* { dg-error ".-mabi=ms. not supported with .-fsanitize=thread." "" { target *-*-* } 0 } */
