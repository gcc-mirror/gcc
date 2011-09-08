/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 20

#undef MASK
#define MASK 0xf1

static void
init_pblendd128 (int *src1, int *src2, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 4; i++)
    {
      src1[i] = (i + seed) * (i + seed) * sign;
      src2[i] = (i + seed + 20) * sign;
      sign = -sign;
    }
}

static void
calc_pblendd128 (int *src1, int *src2, unsigned int mask, int *dst)
{
  int i;

  memcpy (dst, src1, 16);
  for (i = 0; i < 4; i++)
    if (mask & (1 << i))
      dst[i] = src2[i];
}

static void
avx2_test (void)
{
  union128i_d src1, src2, dst;
  int dst_ref[4];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pblendd128 (src1.a, src2.a, i);

      dst.x = _mm_blend_epi32 (src1.x, src2.x, MASK);
      calc_pblendd128 (src1.a, src2.a, MASK, dst_ref);

      if (check_union128i_d (dst, dst_ref))
	abort ();
    }
}
