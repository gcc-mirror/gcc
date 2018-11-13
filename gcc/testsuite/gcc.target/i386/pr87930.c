/* { dg-do compile { target lp64 } } */
/* { dg-options "-fsanitize=address -mabi=ms" } */

int i;

/* { dg-error ".-mabi=ms. not supported with .-fsanitize=address." "" { target *-*-* } 0 } */
