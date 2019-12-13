/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=512" } */

#include <stdint.h>

void
f1 (uint64_t *restrict dst, uint32_t *src1, uint16_t *src2, uint32_t *index)
{
  for (int i = 0; i < 7; ++i)
    dst[i] += (uint32_t) (src1[i] + src2[index[i]]);
}

void
f2 (uint64_t *restrict dst, uint32_t *src1, uint16_t *src2, uint64_t *index)
{
  for (int i = 0; i < 7; ++i)
    dst[i] += (uint32_t) (src1[i] + src2[index[i]]);
}

void
f3 (uint64_t *restrict dst, uint32_t *src1, uint16_t **src2)
{
  for (int i = 0; i < 7; ++i)
    dst[i] += (uint32_t) (src1[i] + *src2[i]);
}

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d, p[0-7]/z, \[x2, z[0-9]+\.d, lsl 1\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d, p[0-7]/z, \[z[0-9]+\.d\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d, p[0-7]/z, \[x1\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d, p[0-7]/z, \[x3\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x0\]\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x2\]\n} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x3\]\n} 1 } } */

/* { dg-final { scan-assembler-times {\tadd\tz} 6 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 3 } } */

/* { dg-final { scan-assembler-times {\tuxt.\t} 3 } } */
/* { dg-final { scan-assembler-times {\tuxtw\tz[0-9]+\.d,} 3 } } */
