/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"


#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (int *mask, int *src1, int *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      dst[i] = src1[mask[i] & 15];
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = (i + 10) * (i + 10) * sign;
      src2.a[i] = (i + 30);
      sign = -sign;
      res3.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permutexvar_epi32) (src1.x, src2.x);
  res2.x = INTRINSIC (_maskz_permutexvar_epi32) (mask, src1.x, src2.x);
  res3.x = INTRINSIC (_mask_permutexvar_epi32) (res3.x, mask, src1.x, src2.x);

  CALC (src1.a, src2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
