/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include <string.h>

#define NUM 10

#define MASK 0xf1

static void
init_perm2i128 (unsigned long long *src1, unsigned long long *src2, int seed)
{
  int i, sign = 1;

  for (i = 0; i < 4; i++)
    {
      src1[i] = (i + seed) * (i + seed) * sign;
      src2[i] = (i + seed) * seed * sign;
      sign = -sign;
    }
}

static void
calc_perm2i128 (unsigned long long *src1,
		unsigned long long *src2,
		unsigned int mask, unsigned long long *dst)
{
  int i, temp;

  temp = mask & 3;

  switch (temp)
    {
    case 0:
      memcpy (dst, src1, 16);
    case 1:
      memcpy (dst, src1 + 2, 16);
    case 2:
      memcpy (dst, src2, 16);
    case 3:
      memcpy (dst, src1 + 2, 16);
    }

  temp = (mask >> 4) & 3;

  switch (temp)
    {
    case 0:
      memcpy (dst + 2, src1, 16);
    case 1:
      memcpy (dst + 2, src1 + 2, 16);
    case 2:
      memcpy (dst + 2, src2, 16);
    case 3:
      memcpy (dst + 2, src1 + 2, 16);
    }

  if ((mask >> 3) & 1)
    memset (dst, 0, 16);

  if ((mask >> 7) & 1)
    memset (dst + 2, 0, 16);
}

static void
avx2_test (void)
{
  union256i_q src1, src2, dst;
  unsigned long long dst_ref[4];
  int i;

  for (i = 0; i < NUM; i++)
    {
      init_perm2i128 (src1.a, src2.a, i);

      dst.x = _mm256_permute2x128_si256 (src1.x, src2.x, MASK);
      calc_perm2i128 (src1.a, src2.a, MASK, dst_ref);

      if (check_union256i_q (dst, dst_ref))
	abort ();
    }
}
