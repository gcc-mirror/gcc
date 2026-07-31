/* { dg-do compile } */
/* { dg-options "-O2 -freciprocal-math -ffinite-math-only -fdump-tree-optimized" } */
/* { dg-additional-options "-fassociative-math -fno-signed-zeros -fno-trapping-math" } */

/* (A / B) * (C / D) is (A * C) / (B * D): one of the two divisions becomes
   a multiply.  The rule also needs infinities and NaNs excluded, because the
   two products it forms can leave the range of the type.  */

double f1 (double a, double b, double c)
{
  return (a / b) * (1.0 / c);
}

double f2 (double a, double b, double c, double d)
{
  return (a / b) * (c / d);
}

float f3 (float a, float b, float c, float d)
{
  return (a / b) * (c / d);
}

/* Trapping math must preserve both divisions.  */
__attribute__((optimize ("trapping-math")))
double trapping (double a, double b, double c, double d)
{
  return (a / b) * (c / d);
}

/* Must not fold: the reciprocal is shared, so the multiplications should
   reuse it rather than pay for a second division.  */
double keep (double a, double b, double c, double *r)
{
  double t = 1.0 / c;
  r[0] = (a / b) * t;
  r[1] = t;
  return t;
}

/* Must not fold without -ffinite-math-only: see recip-mult-div-2.c.  */

/* { dg-final { scan-tree-dump-times " / " 7 "optimized" } } */
