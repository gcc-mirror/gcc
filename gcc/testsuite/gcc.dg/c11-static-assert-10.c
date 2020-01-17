/* Test for constant expressions: casts with integer overflow.  PR
   c/93241.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <limits.h>

_Static_assert (0 ? (_Bool) (INT_MAX + 1) : 1, "");
_Static_assert (0 ? (short) ((INT_MAX + 1) != 0) : 1, "");
