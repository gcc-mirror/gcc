/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 10

static void
init_pbroadcastd128 (int *src, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 4; i++)
    {
      src[i] = (i + seed) * (i + seed) * sign;
      sign = -sign;
    }
}

static void
calc_pbroadcastd128 (int *src, int *dst)
{
  int i;

  for (i = 0; i < 4; i++)
    dst[i] = src[0];
}

static void
avx2_test (void)
{
  union128i_d src, dst;
  int dst_ref[4];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pbroadcastd128 (src.a, i);

      dst.x = _mm_broadcastd_epi32 (src.x);
      calc_pbroadcastd128 (src.a, dst_ref);

      if (check_union128i_d (dst, dst_ref))
	abort ();
    }
}
