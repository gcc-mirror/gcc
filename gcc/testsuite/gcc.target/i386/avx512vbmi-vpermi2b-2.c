/* { dg-do run } */
/* { dg-options "-O2 -mavx512vbmi" } */
/* { dg-require-effective-target avx512vbmi } */

#define AVX512VBMI

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#include <math.h>
#include <limits.h>
#include <float.h>
#include "avx512f-mask-type.h"

#define NUM 32

void
CALC (char *dst, char *src1, char *ind, char *src2)
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
  UNION_TYPE (AVX512F_LEN, i_b) s1, s2, res, ind;
  char res_ref[SIZE];

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
	INTRINSIC (_mask2_permutex2var_epi8) (s1.x, ind.x, mask,
					       s2.x);

      MASK_MERGE (i_b) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, i_b) (res, res_ref))
	abort ();
    }
}
