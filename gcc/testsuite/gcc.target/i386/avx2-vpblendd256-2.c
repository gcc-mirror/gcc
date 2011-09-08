/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 20

#undef MASK
#define MASK 0xf1

static void
init_pblendd256 (int *src1, int *src2, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 8; i++)
    {
      src1[i] = (i + seed) * (i + seed) * sign;
      src2[i] = (i + seed + 20) * sign;
      sign = -sign;
    }
}

static void
calc_pblendd256 (int *src1, int *src2, unsigned int mask, int *dst)
{
  int i;

  memcpy (dst, src1, 32);
  for (i = 0; i < 8; i++)
    if (mask & (1 << i))
      dst[i] = src2[i];
}

static void
avx2_test (void)
{
  union256i_d src1, src2, dst;
  int dst_ref[8];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pblendd256 (src1.a, src2.a, i);

      dst.x = _mm256_blend_epi32 (src1.x, src2.x, MASK);
      calc_pblendd256 (src1.a, src2.a, MASK, dst_ref);

      if (check_union256i_d (dst, dst_ref))
	abort ();
    }
}
