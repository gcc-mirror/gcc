/* Test C11 static assertions.  Omitting the string diagnosed with
   -Wc11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x -Wc11-c2x-compat" } */

_Static_assert (1); /* { dg-warning "does not support omitting the string" } */
