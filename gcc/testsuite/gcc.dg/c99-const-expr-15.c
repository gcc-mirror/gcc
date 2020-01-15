/* Test for constant expressions: casts with integer overflow.  PR
   c/93241.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

#include <limits.h>

struct s { int a : (0 ? (_Bool) (INT_MAX + 1) : 1); };
struct t { int a : (0 ? (short) ((INT_MAX + 1) != 0) : 1); };
