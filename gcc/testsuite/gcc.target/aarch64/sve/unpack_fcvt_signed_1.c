/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline" } */

#include <stdint.h>

void __attribute__ ((noinline, noclone))
unpack_double_int_plus8 (double *d, int32_t *s, int size)
{
  for (int i = 0; i < size; i++)
    d[i] = s[i] + 8;
}

/* { dg-final { scan-assembler-times {\tzip1\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tzip2\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
