/* Test C23 static assertions.  Omitting the string supported.  Failed
   assertions.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

_Static_assert (0); /* { dg-error "static assertion failed" } */
