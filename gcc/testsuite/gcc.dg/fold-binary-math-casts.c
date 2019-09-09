/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

#include <math.h>

float
f (float x, float y)
{
  double z = 1.0 / x;
  return z * y;
}

float
g (float x, float y)
{
  double a = 1.0 / x;
  double b = 1.0 / y;
  long double k = x*x*x*x*x*x;

  return a + b - k;
}

float
h (float x)
{
  double a = x * 2.0;
  double b = a / 3.5f;
  return a + b;
}

float
i (float y, float z)
{
  return pow (y, 2.0) / (double) (y + z);
}

float
j (float x, float y)
{
  double t = 4.0 * x;
  double z = t + y;
  return z;
}

float
k (float a)
{
  return 1.0 / sqrtf (a);
}

float
l (float a)
{
  return (double) a * (a / 2.0);
}

/* { dg-final { scan-tree-dump-not "\\(double\\)" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\(float\\)" "optimized" } } */
