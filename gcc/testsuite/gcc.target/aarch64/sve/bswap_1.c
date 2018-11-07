/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void
f (uint16_t *a, uint16_t *b)
{
  for (int i = 0; i < 100; ++i)
    a[i] = __builtin_bswap16 (b[i]);
}

/* { dg-final { scan-assembler-times {\trevb\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 1 { xfail aarch64_big_endian } } } */
