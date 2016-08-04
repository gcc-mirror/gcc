/* { dg-do compile } */
/* { dg-require-effective-target large_long_double } */
/* { dg-options "-O2 -ffast-math -fdump-tree-forwprop-details" } */

#include <math.h>

long double f3 (long double x)
{
  long double t1 = fabsl (x);
  long double t2 = t1 / x; 
  return t2;
}

/* { dg-final { scan-tree-dump "__builtin_copysignl" "forwprop1" } } */
