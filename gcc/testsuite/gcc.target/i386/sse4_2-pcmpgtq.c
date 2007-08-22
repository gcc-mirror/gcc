/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.2" } */

#include "sse4_2-check.h"

#include <nmmintrin.h>

#define NUM 64

static void
sse4_2_test (void)
{
  union
    {
      __m128i x[NUM / 2];
      long long ll[NUM];
    } dst, src1, src2;
  int i, sign = 1;
  long long is_eq;

  for (i = 0; i < NUM; i++)
    {
      src1.ll[i] = i * i * sign;
      src2.ll[i] = (i + 20) * sign;
      sign = -sign;
    }

  for (i = 0; i < NUM; i += 2)
    dst.x[i / 2] = _mm_cmpgt_epi64 (src1.x[i / 2], src2.x[i / 2]);

  for (i = 0; i < NUM; i++)
    {
      is_eq = src1.ll[i] > src2.ll[i] ? 0xFFFFFFFFFFFFFFFFLL : 0LL;
      if (is_eq != dst.ll[i])
	abort ();
    }
}
