/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void f64_i32 (double *restrict x, int32_t  *restrict y, int n)
{
  for (int i = 0; i < n; i++)
    x[i] = (double)y[i];
}

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.[sd], p[0-7]/m, z[0-9]+\.d\n} 1 } } */
