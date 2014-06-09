/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (unsigned int *r, unsigned int *s1, int count)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = count < 32 ? (s1[i] >> count) : 0;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_ud) res1, res2, res3, src1;
  MASK_TYPE mask = MASK_VALUE;
  unsigned int res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + 7 * i % 291;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_srli_epi32) (src1.x, 3);
  res2.x =
    INTRINSIC (_mask_srli_epi32) (res2.x, mask, src1.x, 3);
  res3.x = INTRINSIC (_maskz_srli_epi32) (mask, src1.x, 3);

  CALC (res_ref, src1.a, 3);

  if (UNION_CHECK (AVX512F_LEN, i_ud) (res1, res_ref))
    abort ();

  MASK_MERGE (i_ud) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_ud) (res2, res_ref))
    abort ();

  MASK_ZERO (i_ud) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_ud) (res3, res_ref))
    abort ();


  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_srli_epi32) (src1.x, 33);
  res2.x =
    INTRINSIC (_mask_srli_epi32) (res2.x, mask, src1.x, 33);
  res3.x = INTRINSIC (_maskz_srli_epi32) (mask, src1.x, 33);

  CALC (res_ref, src1.a, 33);

  if (UNION_CHECK (AVX512F_LEN, i_ud) (res1, res_ref))
    abort ();

  MASK_MERGE (i_ud) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_ud) (res2, res_ref))
    abort ();

  MASK_ZERO (i_ud) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_ud) (res3, res_ref))
    abort ();
}
