/* { dg-options "-O3 -msve-vector-bits=256" } */

#include <stdint.h>

void
f1 (uint64_t *restrict ptr1, uint8_t *restrict ptr2, uint8_t start)
{
#pragma GCC unroll 0
  for (int i = 0; i < 4; ++i)
    {
      ptr1[i] = 10;
      ptr2[i] = start;
      start += 1;
    }
}

void
f2 (uint64_t *restrict ptr1, uint16_t *restrict ptr2, uint16_t start)
{
#pragma GCC unroll 0
  for (int i = 0; i < 4; ++i)
    {
      ptr1[i] = 10;
      ptr2[i] = start;
      start += 2;
    }
}

void
f3 (uint64_t *restrict ptr1, uint32_t *restrict ptr2, uint32_t start)
{
#pragma GCC unroll 0
  for (int i = 0; i < 4; ++i)
    {
      ptr1[i] = 10;
      ptr2[i] = start;
      start += 4;
    }
}

/* { dg-final { scan-assembler {\tindex\tz[0-9]+\.d, x[0-9]+, #1\n} } } */
/* { dg-final { scan-assembler {\tindex\tz[0-9]+\.d, x[0-9]+, #1\n} } } */
/* { dg-final { scan-assembler {\tindex\tz[0-9]+\.d, x[0-9]+, #4\n} } } */

/* { dg-final { scan-assembler-not {\tindex\tz[0-9]+\.d, w[0-9]+, #1\n} } } */
/* { dg-final { scan-assembler-not {\tindex\tz[0-9]+\.d, w[0-9]+, #1\n} } } */
/* { dg-final { scan-assembler-not {\tindex\tz[0-9]+\.d, w[0-9]+, #4\n} } } */
