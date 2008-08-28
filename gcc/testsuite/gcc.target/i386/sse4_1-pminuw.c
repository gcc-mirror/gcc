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
      __m128i x[NUM / 8];
      unsigned short i[NUM];
    } dst, src1, src2;
  int i;
  unsigned short min;

  for (i = 0; i < NUM; i++)
    {
      src1.i[i] = i * i;
      src2.i[i] = i + 20;
      if ((i % 8))
	src2.i[i] |= 0x8000;
    }

  for (i = 0; i < NUM; i += 8)
    dst.x[i / 8] = _mm_min_epu16 (src1.x[i / 8], src2.x[i / 8]);

  for (i = 0; i < NUM; i++)
    {
      min = src1.i[i] >= src2.i[i] ? src2.i[i] : src1.i[i];
      if (min != dst.i[i])
	abort ();
    }
}
