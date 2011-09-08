/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 10

static void
init_pbroadcastq128 (long long int *src, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 2; i++)
    {
      src[i] = (i + seed) * (i + seed) * sign;
      sign = -sign;
    }
}

static void
calc_pbroadcastq128 (long long int *src, long long int *dst)
{
  int i;

  for (i = 0; i < 2; i++)
    dst[i] = src[0];
}

static void
avx2_test (void)
{
  union128i_q src, dst;
  long long int dst_ref[2];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_pbroadcastq128 (src.a, i);

      dst.x = _mm_broadcastq_epi64 (src.x);
      calc_pbroadcastq128 (src.a, dst_ref);

      if (check_union128i_q (dst, dst_ref))
	abort ();
    }
}
