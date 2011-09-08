/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define MASK 0x1a

#define NUM 10

static void
init_permpd (double *src1, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 4; i++)
    {
      src1[i] = (i + seed) * (i + seed) * sign;
      sign = -sign;
    }
}

static void
calc_permpd (double *src1, int mask, double *dst)
{
  int i;
  unsigned temp;

  memcpy (dst, src1, 32);
  for (i = 0; i < 4; i++)
    {
      temp = mask >> (i * 2);
      dst[i] = src1[temp & 3];
    }
}

static void
avx2_test (void)
{
  union256d src1, dst;
  double dst_ref[4];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_permpd (src1.a, i);

      dst.x = _mm256_permute4x64_pd (src1.x, MASK);
      calc_permpd (src1.a, MASK, dst_ref);

      if (check_union256d (dst, dst_ref))
	abort ();
    }
}
