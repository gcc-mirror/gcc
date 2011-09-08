/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 10

static void
init_pbroadcastd256 (int *src, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 4; i++)
    {
      src[i] = (i + seed) * (i + seed) * sign;
      sign = -sign;
    }
}

static void
calc_pbroadcastd256 (int *src, int *dst)
{
  int i;

  for (i = 0; i < 8; i++)
    dst[i] = src[0];
}

static void
avx2_test (void)
{
  union128i_d src;
  union256i_d dst;
  int dst_ref[8];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pbroadcastd256 (src.a, i);

      dst.x = _mm256_broadcastd_epi32 (src.x);
      calc_pbroadcastd256 (src.a, dst_ref);

      if (check_union256i_d (dst, dst_ref))
	abort ();
    }
}
