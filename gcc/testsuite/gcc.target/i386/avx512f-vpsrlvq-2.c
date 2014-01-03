/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

CALC (unsigned long long *r, unsigned long long *s1,
      unsigned long long *s2)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = s2[i] < 64 ? (s1[i] >> s2[i]) : 0;
    }
}

void static
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned long long res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + 7 * i % 291;
      src2.a[i] = 1 + 17 * i % 71;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_srlv_epi64) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_srlv_epi64) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_srlv_epi64) (mask, src1.x, src2.x);

  CALC (res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
