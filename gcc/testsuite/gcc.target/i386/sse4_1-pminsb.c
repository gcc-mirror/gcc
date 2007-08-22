/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>

#define NUM 1024

static void
sse4_1_test (void)
{
  union
    {
      __m128i x[NUM / 16];
      char i[NUM];
    } dst, src1, src2;
  int i, sign = 1;
  char min;

  for (i = 0; i < NUM; i++)
    {
      src1.i[i] = i * i * sign;
      src2.i[i] = (i + 20) * sign;
      sign = -sign;
    }

  for (i = 0; i < NUM; i += 16)
    dst.x[i / 16] = _mm_min_epi8 (src1.x[i / 16], src2.x[i / 16]);

  for (i = 0; i < NUM; i++)
    {
      min = src1.i[i] >= src2.i[i] ? src2.i[i] : src1.i[i];
      if (min != dst.i[i])
	abort ();
    }
}
