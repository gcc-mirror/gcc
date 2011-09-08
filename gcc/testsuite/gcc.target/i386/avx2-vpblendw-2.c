/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 20

#undef MASK
#define MASK 0xfe

static void
init_pblendw (short *src1, short *src2, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 16; i++)
    {
      src1[i] = (i + seed) * (i + seed) * sign;
      src2[i] = (i + seed + 20) * sign;
      sign = -sign;
    }
}

static void
calc_pblendw (short *src1, short *src2, unsigned int mask, short *dst)
{
  int i;

  memcpy (dst, src1, 32);
  for (i = 0; i < 16; i++)
    if (mask & (1 << (i % 8)))
      dst[i] = src2[i];
}

static void
avx2_test (void)
{
  union256i_w src1, src2, dst;
  short dst_ref[16];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pblendw (src1.a, src2.a, i);

      dst.x = _mm256_blend_epi16 (src1.x, src2.x, MASK);
      calc_pblendw (src1.a, src2.a, MASK, dst_ref);

      if (check_union256i_w (dst, dst_ref))
	abort ();
    }
}
