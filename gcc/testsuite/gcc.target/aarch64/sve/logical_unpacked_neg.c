/* { dg-options "-O3 -msve-vector-bits=256" } */

#include <stdint.h>
#include <stdlib.h>

void
f (uint32_t *restrict dst, int8_t *restrict src)
{
  for (int i = 0; i < 7; ++i)
    dst[i] = (int8_t) -src[i];
}

/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tneg\tz[0-9]+\.b, p[0-9]+/m, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsxtb\tz[0-9]+\.s,} 1 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 1 } } */
