/* { dg-do compile } */
/* { dg-options "-fsanitize-recover=unreachable" } */

int i;

/* { dg-error "-fsanitize-recover=unreachable is not supported" "" { target *-*-* } 0 } */
