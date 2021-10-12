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
      __m128i x[NUM / 2];
      unsigned long long ll[NUM];
      unsigned int i[NUM * 2];
    } dst, src;
  int i;

  for (i = 0; i < NUM; i++)
    {
      src.i[(i % 2) + (i / 2) * 4] = i * i;
      if ((i % 2))
        src.i[(i % 2) + (i / 2) * 4] |= 0x80000000;
    }

  for (i = 0; i < NUM; i += 2)
    dst.x [i / 2] = _mm_cvtepu32_epi64 (src.x [i / 2]);

  for (i = 0; i < NUM; i++)
    if (src.i[(i % 2) + (i / 2) * 4] != dst.ll[i])
      abort ();
}
