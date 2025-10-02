/* { dg-do compile } */
/* { dg-options "-Os -mfpmath=sse -mno-avx -msse4.1" } */

#include <math.h>

float
cf (float x)
{
  return ceilf (x);
}

float
ff (float x)
{
  return floorf (x);
}

float
tf (float x)
{
  return truncf (x);
}

double
c (double x)
{
  return ceil (x);
}

double
f (double x)
{
  return floor (x);
}

double
t (double x)
{
  return trunc (x);
}

/* { dg-final { scan-assembler-times "roundss" 3 } } */
/* { dg-final { scan-assembler-times "roundsd" 3 } } */
