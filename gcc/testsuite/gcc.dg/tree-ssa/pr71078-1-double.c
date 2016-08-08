/* { dg-do compile } */
/* { dg-require-effective-target large_double } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -ffast-math -fdump-tree-forwprop-details" } */

#include <math.h>

double f2(double x)
{
  double t1 = fabs (x);
  double t2 = x / t1;
  return t2;
}

/* { dg-final { scan-tree-dump "__builtin_copysign" "forwprop1" } } */
