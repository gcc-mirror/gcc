/* Test C2x static assertions.  Omitting the string supported.  Failed
   assertions.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic" } */

_Static_assert (0); /* { dg-error "static assertion failed" } */
