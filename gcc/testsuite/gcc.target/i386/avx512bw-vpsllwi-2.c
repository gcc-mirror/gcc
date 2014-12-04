/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *r, short *s1, short count)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = count < 16 ? (s1[i] << count) : 0;
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3, src1;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + sign * 7 * i % 291;
      sign = sign * -1;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_slli_epi16) (src1.x, 5);
  res2.x = INTRINSIC (_mask_slli_epi16) (res2.x, mask, src1.x, 5);
  res3.x = INTRINSIC (_maskz_slli_epi16) (mask, src1.x, 5);

  CALC (res_ref, src1.a, 5);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();


  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_slli_epi16) (src1.x, 17);
  res2.x = INTRINSIC (_mask_slli_epi16) (res2.x, mask, src1.x, 17);
  res3.x = INTRINSIC (_maskz_slli_epi16) (mask, src1.x, 17);

  CALC (res_ref, src1.a, 17);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
