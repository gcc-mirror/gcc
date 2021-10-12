/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

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
      __m128i x[NUM / 8];
      short s[NUM];
      signed char c[NUM * 2];
    } dst, src;
  int i, sign = 1;

  for (i = 0; i < NUM; i++)
    {
      src.c[(i % 8) + (i / 8) * 16] = i * i * sign;
      sign = -sign;
    }

  for (i = 0; i < NUM; i += 8)
    dst.x [i / 8] = _mm_cvtepi8_epi16 (src.x [i / 8]);

  for (i = 0; i < NUM; i++)
    if (src.c[(i % 8) + (i / 8) * 16] != dst.s[i])
      abort ();
}
