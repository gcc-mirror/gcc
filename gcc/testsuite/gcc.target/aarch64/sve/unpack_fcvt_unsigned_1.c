/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize --param aarch64-vect-compare-costs=0" } */

#include <stdint.h>

void __attribute__ ((noinline, noclone))
unpack_double_int_plus9 (double *d, uint32_t *s, int size)
{
  for (int i = 0; i < size; i++)
    d[i] = (double) (s[i] + 9);
}

/* { dg-final { scan-assembler-times {\tzip1\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tzip2\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
