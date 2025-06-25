/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize --param aarch64-vect-compare-costs=0" } */

#include <stdint.h>

void __attribute__ ((noinline, noclone))
pack_int_double_plus_3 (int32_t *d, double *s, int size)
{
  for (int i = 0; i < size; i++)
    d[i] = s[i] + 3;
}

/* { dg-final { scan-assembler-times {\tfcvtzs\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuzp1\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
