/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

double t1 (double *x, int N)
{
  double result = 0.0;

  #pragma simd reduction (max: result)
  for (int i = 0; i < N; ++i)
    result = x[i] > result ? x[i] : result;

  return result;
}
