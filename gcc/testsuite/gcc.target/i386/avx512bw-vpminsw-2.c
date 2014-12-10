/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)

#include "avx512f-mask-type.h"

void
CALC (short *src1, short *src2, short *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    dst[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_w) src1, src2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] =  i * sign;
      src2.a[i] = (i + 20) * sign;
      sign = -sign;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_min_epi16) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_min_epi16) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_min_epi16) (mask, src1.x, src2.x);

  CALC (src1.a, src2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
