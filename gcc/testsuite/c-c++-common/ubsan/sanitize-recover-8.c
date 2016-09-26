/* { dg-do compile } */
/* { dg-options "-fsanitize=leak -fsanitize=thread" } */

int i;

/* { dg-error "-fsanitize=leak is incompatible with -fsanitize=thread" "" { target *-*-* } 0 } */
