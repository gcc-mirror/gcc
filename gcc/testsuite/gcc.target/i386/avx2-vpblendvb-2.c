/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 20

static void
init_pblendb (char *src1, char *src2, char *mask, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 32; i++)
    {
      src1[i] = (i + seed) * (i + seed) * sign;
      src2[i] = (i + seed + 20) * sign;
      sign = -sign;

      if (sign > 0)
	mask[i] = 1 << 7;
      else
	mask[i] = 0;
    }
}

static void
calc_pblendb (char *src1, char *src2, char *mask, char *dst)
{
  int i;

  memcpy (dst, src1, 32);
  for (i = 0; i < 32; i++)
    if (mask[i] & (1 << 7))
      dst[i] = src2[i];
}

static void
avx2_test (void)
{
  union256i_b src1, src2, mask, dst;
  char dst_ref[32];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pblendb (src1.a, src2.a, mask.a, i);

      dst.x = _mm256_blendv_epi8 (src1.x, src2.x, mask.x);
      calc_pblendb (src1.a, src2.a, mask.a, dst_ref);

      if (check_union256i_b (dst, dst_ref))
	abort ();
    }
}
