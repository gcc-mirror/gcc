/* Test for static assertions not in C99.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

_Static_assert (1, ""); /* { dg-error "ISO C99 does not support '_Static_assert'" } */
