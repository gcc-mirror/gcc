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

#define NUM 128

static void
TEST (void)
{
  union
    {
      __m128i x[NUM / 2];
      long long ll[NUM];
      int i[NUM * 2];
    } dst, src;
  int i, sign = 1;

  for (i = 0; i < NUM; i++)
    {
      src.i[(i % 2) + (i / 2) * 4] = i * i * sign;
      sign = -sign;
    }

  for (i = 0; i < NUM; i += 2)
    dst.x [i / 2] = _mm_cvtepi32_epi64 (src.x [i / 2]);

  for (i = 0; i < NUM; i++)
    if (src.i[(i % 2) + (i / 2) * 4] != dst.ll[i])
      abort ();
}
