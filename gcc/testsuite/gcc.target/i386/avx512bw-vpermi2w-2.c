/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include <math.h>
#include <limits.h>
#include <float.h>
#include "avx512f-mask-type.h"

#define NUM 32

void
CALC (short *dst, short *src1, short *ind, short *src2)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      unsigned long long offset = ind[i] & (SIZE - 1);
      unsigned long long cond = ind[i] & SIZE;

      dst[i] = cond ? src2[offset] : src1[offset];
    }
}

void
TEST (void)
{
  int i, j;
  UNION_TYPE (AVX512F_LEN, i_w) s1, s2, res, ind;
  short res_ref[SIZE];

  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < NUM; i++)
    {
      for (j = 0; j < SIZE; j++)
	{
	  ind.a[j] = DEFAULT_VALUE;
	  s1.a[j] = i * 2 * j + 1;
	  s2.a[j] = i * 2 * j;

	  res.a[j] = DEFAULT_VALUE;
	}

      CALC (res_ref, s1.a, ind.a, s2.a);

      res.x =
	INTRINSIC (_mask2_permutex2var_epi16) (s1.x, ind.x, mask,
					       s2.x);

      MASK_MERGE (i_w) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, i_w) (res, res_ref))
	abort ();
    }
}
