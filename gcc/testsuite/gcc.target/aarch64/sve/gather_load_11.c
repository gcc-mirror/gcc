/* { dg-do compile } */
/* { dg-options "-O3 -fno-vect-cost-model" } */

#include <stdint.h>

void
f1 (int32_t *restrict y, int32_t *restrict x, int32_t *restrict index)
{
  for (int i = 0; i < 100; ++i)
    {
      y[i * 2] = x[index[i * 2]] + 1;
      y[i * 2 + 1] = x[index[i * 2 + 1]] + 2;
    }
}

void
f2 (int32_t *restrict y, int32_t *restrict x, uint32_t *restrict index)
{
  for (int i = 0; i < 100; ++i)
    {
      y[i * 2] = x[index[i * 2]] + 1;
      y[i * 2 + 1] = x[index[i * 2 + 1]] + 2;
    }
}

void
f3 (int32_t *restrict y, int32_t *restrict x, uint64_t *restrict index)
{
  for (int i = 0; i < 100; ++i)
    {
      y[i * 2] = x[index[i * 2]] + 1;
      y[i * 2 + 1] = x[index[i * 2 + 1]] + 2;
    }
}

void
f4 (int64_t *restrict y, int64_t *restrict x, uint64_t *restrict index)
{
  for (int i = 0; i < 100; ++i)
    {
      y[i * 2] = x[index[i * 2]] + 1;
      y[i * 2 + 1] = x[index[i * 2 + 1]] + 2;
    }
}

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+\.s, sxtw #?2\]} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z, \[x[0-9]+, z[0-9]+\.s, uxtw #?2\]} 1 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+\.d, lsl #?2\]} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z, \[x[0-9]+, z[0-9]+\.d, lsl #?3\]} 1 } } */
