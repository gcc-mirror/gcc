/* Test C11 static assertions.  Omitting the string diagnosed with
   -Wc11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -Wc11-c23-compat" } */

_Static_assert (1); /* { dg-warning "does not support omitting the string" } */
