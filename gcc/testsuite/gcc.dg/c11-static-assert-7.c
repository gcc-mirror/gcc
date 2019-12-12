/* Test C11 static assertions.  Omitting the string not supported.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

_Static_assert (1); /* { dg-warning "does not support omitting the string" } */
