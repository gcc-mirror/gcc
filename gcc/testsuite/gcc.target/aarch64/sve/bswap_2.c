/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

void
f (uint32_t *a, uint32_t *b)
{
  for (int i = 0; i < 100; ++i)
    a[i] = __builtin_bswap32 (b[i]);
}

/* { dg-final { scan-assembler-times {\trevb\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 { xfail aarch64_big_endian } } } */
