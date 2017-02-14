/* { dg-do compile } */
/* { dg-options "-fsanitize-recover=return" } */

int i;

/* { dg-error "-fsanitize-recover=return is not supported" "" { target *-*-* } 0 } */
