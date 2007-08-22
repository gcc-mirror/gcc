/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>

#define NUM 128

static void
sse4_1_test (void)
{
  union
    {
      __m128i x[NUM / 4];
      int i[NUM];
      short s[NUM * 2];
    } dst, src;
  int i, sign = 1;

  for (i = 0; i < NUM; i++)
    {
      src.s[(i % 4) + (i / 4) * 8] = i * i * sign;
      sign = -sign;
    }

  for (i = 0; i < NUM; i += 4)
    dst.x [i / 4] = _mm_cvtepi16_epi32 (src.x [i / 4]);

  for (i = 0; i < NUM; i++)
    if (src.s[(i % 4) + (i / 4) * 8] != dst.i[i])
      abort ();
}
