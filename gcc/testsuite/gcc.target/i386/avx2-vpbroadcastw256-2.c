/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 10

static void
init_pbroadcastw256 (short *src, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 8; i++)
    {
      src[i] = (i + seed) * (i + seed) * sign;
      sign = -sign;
    }
}

static void
calc_pbroadcastw256 (short *src, short *dst)
{
  int i;

  for (i = 0; i < 16; i++)
    dst[i] = src[0];
}

static void
avx2_test (void)
{
  union128i_w src;
  union256i_w dst;
  short dst_ref[16];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pbroadcastw256 (src.a, i);

      dst.x = _mm256_broadcastw_epi16 (src.x);
      calc_pbroadcastw256 (src.a, dst_ref);

      if (check_union256i_w (dst, dst_ref))
	abort ();
    }
}
