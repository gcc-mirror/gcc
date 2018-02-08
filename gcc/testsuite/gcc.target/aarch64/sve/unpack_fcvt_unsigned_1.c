/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void __attribute__ ((noinline, noclone))
unpack_double_int_plus9 (double *d, uint32_t *s, int size)
{
  for (int i = 0; i < size; i++)
    d[i] = (double) (s[i] + 9);
}

/* { dg-final { scan-assembler-times {\tuunpklo\tz[0-9]+\.d, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuunpkhi\tz[0-9]+\.d, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
