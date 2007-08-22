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
      unsigned int i[NUM];
      unsigned char c[NUM * 4];
    } dst, src;
  int i;

  for (i = 0; i < NUM; i++)
    {
      src.c[(i % 4) + (i / 4) * 16] = i * i;
      if ((i % 4))
	src.c[(i % 4) + (i / 4) * 16] |= 0x80;
    }

  for (i = 0; i < NUM; i += 4)
    dst.x [i / 4] = _mm_cvtepu8_epi32 (src.x [i / 4]);

  for (i = 0; i < NUM; i++)
    if (src.c[(i % 4) + (i / 4) * 16] != dst.i[i])
      abort ();
}
