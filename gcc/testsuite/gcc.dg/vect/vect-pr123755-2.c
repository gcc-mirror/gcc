/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

double foo (double *a, double *b, double *c)
{
  double result = 0.0;
  for (int i = 0; i < 1024; ++i)
    result += i & 1 ? __builtin_fma (a[i], b[i], c[i]) : 0.0;
  return result;
}
