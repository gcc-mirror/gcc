/* PR tree-optimization/67815 */
/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-reassoc1-details" } */

float
f0 (float x)
{
  return 7.5 * __builtin_copysignf (2.0, x);
}

float
f1 (float x)
{
  return -7.5 * __builtin_copysignf (2.0, x);
}

double
f2 (double x, double y)
{
  return x * ((1.0/12) * __builtin_copysign (1.0, y));
}

double
f3 (double x, double y)
{
  return (x * (-1.0/12)) * __builtin_copysign (1.0, y);
}

double
f4 (double x, double y, double z)
{
  return (x * z) * ((1.0/12) * __builtin_copysign (4.0, y));
}

double
f5 (double x, double y, double z)
{
  return (x * (-1.0/12)) * z * __builtin_copysign (2.0, y);
}

/* { dg-final { scan-tree-dump-times "Optimizing copysign" 6 "reassoc1"} }*/
