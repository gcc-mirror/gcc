/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#define SIZE_HALF (AVX512F_LEN_HALF / 32)

CALC (int *r, long long *s)
{
  int i;
  for (i = 0; i < SIZE_HALF; i++)
    {
      r[i] = (i < SIZE) ? (int) s[i] : 0;
    }
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN_HALF, i_d) res1, res2, res3, res5;
  int res4[SIZE_HALF];
  UNION_TYPE (AVX512F_LEN, i_q) src;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE_HALF];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i * sign;
      sign = sign * -1;
      res2.a[i] = DEFAULT_VALUE;
      res4[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtepi64_epi32) (src.x);
  res2.x = INTRINSIC (_mask_cvtepi64_epi32) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtepi64_epi32) (mask, src.x);
  INTRINSIC (_mask_cvtepi64_storeu_epi32) (res4, mask, src.x);


  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_d) (res2, res_ref))
    abort ();

  if (checkVi (res4, res_ref, SIZE_HALF))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_d) (res3, res_ref))
    abort ();
}
