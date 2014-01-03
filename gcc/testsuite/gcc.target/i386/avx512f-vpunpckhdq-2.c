/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

CALC (int *r, int *s1, int *s2)
{
  int i;
  for (i = 0; i < SIZE / 4; i++)
    {
      r[4 * i] = s1[4 * i + 2];
      r[4 * i + 1] = s2[4 * i + 2];
      r[4 * i + 2] = s1[4 * i + 3];
      r[4 * i + 3] = s2[4 * i + 3];
    }
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 34 * i * sign;
      src1.a[i] = 179 * i;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_unpackhi_epi32) (src1.x, src2.x);
  res2.x =
    INTRINSIC (_mask_unpackhi_epi32) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_unpackhi_epi32) (mask, src1.x, src2.x);

  CALC (res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
