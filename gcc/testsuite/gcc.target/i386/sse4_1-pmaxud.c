/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>

#define NUM 64

static void
sse4_1_test (void)
{
  union
    {
      __m128i x[NUM / 4];
      unsigned int i[NUM];
    } dst, src1, src2;
  int i;
  unsigned int max;

  for (i = 0; i < NUM; i++)
    {
      src1.i[i] = i * i;
      src2.i[i] = i + 20;
      if ((i % 4))
	src2.i[i] |= 0x80000000;
    }

  for (i = 0; i < NUM; i += 4)
    dst.x[i / 4] = _mm_max_epu32 (src1.x[i / 4], src2.x[i / 4]);

  for (i = 0; i < NUM; i++)
    {
      max = src1.i[i] <= src2.i[i] ? src2.i[i] : src1.i[i];
      if (max != dst.i[i])
	abort ();
    }
}
