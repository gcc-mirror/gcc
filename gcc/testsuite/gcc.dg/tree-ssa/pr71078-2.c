/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-forwprop-details" } */

#include <math.h>

float f1(float x)
{
  float t1 = fabsf (x);
  float t2 = t1 / x; 
  return t2;
}
 
double f2(double x)
{
  double t1 = fabs (x);
  double t2 = t1 / x; 
  return t2;
}

long double f3 (long double x)
{
  long double t1 = fabsl (x);
  long double t2 = t1 / x; 
  return t2;
}

/* { dg-final { scan-tree-dump "__builtin_copysignf" "forwprop1" } } */
/* { dg-final { scan-tree-dump "__builtin_copysign" "forwprop1" } } */
/* { dg-final { scan-tree-dump "__builtin_copysignl" "forwprop1" } } */
