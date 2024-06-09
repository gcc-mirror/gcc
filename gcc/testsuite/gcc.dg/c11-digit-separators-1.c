/* Test C23 digit separators not in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#define m(x) 0

_Static_assert (m(1'2)+(3'4) == 0, "digit separators");
