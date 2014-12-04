/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *r, short *s1, short *s2)
{
  int i;
  for (i = 0; i < SIZE/8; i++)
    {
      r[8 * i] = s1[8 * i];
      r[8 * i + 1] = s2[8 * i];
      r[8 * i + 2] = s1[8 * i + 1];
      r[8 * i + 3] = s2[8 * i + 1];
      r[8 * i + 4] = s1[8 * i + 2];
      r[8 * i + 5] = s2[8 * i + 2];
      r[8 * i + 6] = s1[8 * i + 3];
      r[8 * i + 7] = s2[8 * i + 3];
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 34 * i * sign;
      src1.a[i] = 179 * i;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_unpacklo_epi16) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_unpacklo_epi16) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_unpacklo_epi16) (mask, src1.x, src2.x);

  CALC (res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
