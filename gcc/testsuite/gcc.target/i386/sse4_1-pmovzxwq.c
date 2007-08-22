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
      __m128i x[NUM / 2];
      unsigned long long ll[NUM];
      unsigned short s[NUM * 4];
    } dst, src;
  int i;

  for (i = 0; i < NUM; i++)
    {
      src.s[(i % 2) + (i / 2) * 8] = i * i;
      if ((i % 2))
	src.s[(i % 2) + (i / 2) * 8] |= 0x8000;
    }

  for (i = 0; i < NUM; i += 2)
    dst.x [i / 2] = _mm_cvtepu16_epi64 (src.x [i / 2]);

  for (i = 0; i < NUM; i++)
    if (src.s[(i % 2) + (i / 2) * 8] != dst.ll[i])
      abort ();
}
