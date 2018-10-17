/* Test C11 static assertions.  Omitting the string not supported.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

_Static_assert (1); /* { dg-error "does not support omitting the string" } */
