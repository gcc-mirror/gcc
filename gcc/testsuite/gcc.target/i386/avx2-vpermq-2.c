/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 10

#define MASK 0xf1

static void
init_permq (unsigned long long *src1, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 4; i++)
    {
      src1[i] = (i + seed) * (i + seed) * sign;
      sign = -sign;
    }
}

static void
calc_permq (unsigned long long *src1, unsigned int mask,
	    unsigned long long *dst)
{
  int i, temp;

  for (i = 0; i < 4; i++)
    {
      temp = (mask >> (2 * i)) & 3;
      dst[i] = src1[temp];
    }
}

static void
avx2_test (void)
{
  union256i_q src1, dst;
  unsigned long long dst_ref[4];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_permq (src1.a, i);

      dst.x = _mm256_permute4x64_epi64 (src1.x, MASK);
      calc_permq (src1.a, MASK, dst_ref);

      if (check_union256i_q (dst, dst_ref))
	abort ();
    }
}
