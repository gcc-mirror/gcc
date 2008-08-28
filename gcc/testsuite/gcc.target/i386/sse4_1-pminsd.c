/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>

#define NUM 64

static void
TEST (void)
{
  union
    {
      __m128i x[NUM / 4];
      int i[NUM];
    } dst, src1, src2;
  int i, sign = 1;
  int min;

  for (i = 0; i < NUM; i++)
    {
      src1.i[i] = i * i * sign;
      src2.i[i] = (i + 20) * sign;
      sign = -sign;
    }

  for (i = 0; i < NUM; i += 4)
    dst.x[i / 4] = _mm_min_epi32 (src1.x[i / 4], src2.x[i / 4]);

  for (i = 0; i < NUM; i++)
    {
      min = src1.i[i] >= src2.i[i] ? src2.i[i] : src1.i[i];
      if (min != dst.i[i])
	abort ();
    }
}
