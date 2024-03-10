/* { dg-options "-Ofast -mtune=neoverse-v1" } */

double f(double *restrict x, double *restrict y, int *restrict z)
{
  double res = 0.0;
  for (int i = 0; i < 100; ++i)
    res += x[i] * y[z[i]];
  return res;
}

/* { dg-final { scan-assembler-times {\tld1sw\tz[0-9]+\.d,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d,} 2 } } */
/* { dg-final { scan-assembler-times {\tfmla\tz[0-9]+\.d,} 1 } } */
