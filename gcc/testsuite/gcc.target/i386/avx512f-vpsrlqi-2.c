/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

CALC (unsigned long long *r, unsigned long long *s1,
      unsigned long long count)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = count < 64 ? (s1[i] >> count) : 0;
    }
}

void static
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3, src1;
  MASK_TYPE mask = MASK_VALUE;
  unsigned long long res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + 7 * i % 291;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_srli_epi64) (src1.x, 3);
  res2.x =
    INTRINSIC (_mask_srli_epi64) (res2.x, mask, src1.x, 3);
  res3.x = INTRINSIC (_maskz_srli_epi64) (mask, src1.x, 3);

  CALC (res_ref, src1.a, 3);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();


  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_srli_epi64) (src1.x, 65);
  res2.x =
    INTRINSIC (_mask_srli_epi64) (res2.x, mask, src1.x, 65);
  res3.x = INTRINSIC (_maskz_srli_epi64) (mask, src1.x, 65);

  CALC (res_ref, src1.a, 65);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
