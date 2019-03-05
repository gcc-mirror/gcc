/* { dg-options "-O3" } */

#include <stdint.h>

#define N 55

void __attribute__ ((noipa))
f (double *restrict a, double *restrict b, double *restrict c,
   double *restrict d, double *restrict e, int64_t *restrict cond)
{
  for (int i = 0; i < N; ++i)
    {
      a[i] = cond[i] ? __builtin_fma (c[i], d[i], e[i]) : e[i];
      b[i] = cond[i] ? __builtin_fma (c[i], e[i], d[i]) : d[i];
    }
}

/* { dg-final { scan-assembler-times {\tfmla\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-not {\tfmad\t} } } */
