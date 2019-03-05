/* Test static assertions.  Omitting the string should not result in a
   duplicate diagnostic in pre-C11 modes.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic" } */

_Static_assert (1); /* { dg-warning "ISO C99 does not support '_Static_assert'" } */
