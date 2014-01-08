/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

CALC (int *r, float *s)
{
  int i;
  for (i = 0; i < SIZE; i++)
    r[i] = (s[i] >= 0) ? (int) (s[i] + 0.5) : (int) (s[i] - 0.5);
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN,) src;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      res2.a[i] = DEFAULT_VALUE;
      src.a[i] = 1.5 + 34.67 * i * sign;
      sign = sign * -1;
    }

  res1.x = INTRINSIC (_cvtps_epi32) (src.x);
  res2.x = INTRINSIC (_mask_cvtps_epi32) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtps_epi32) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
