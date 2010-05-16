/* Test for static assertions not in C90.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

_Static_assert (1, ""); /* { dg-error "ISO C90 does not support '_Static_assert'" } */
