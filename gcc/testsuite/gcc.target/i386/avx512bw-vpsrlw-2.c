/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (unsigned short *r, unsigned short *s1, unsigned short *s2)
{
  int i;
  unsigned short count = s2[0];
  for (i = 0; i < SIZE; i++)
    {
      r[i] = count < 16 ? (s1[i] >> count) : 0;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3, src1;
  UNION_TYPE (128, i_w) src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + 7 * i % 291;
    }

  for (i = 0; i < 128 / 16; i++)
    {
      src2.a[i] = 0;
    }

  src2.a[0] = 1;
  src2.a[1] = 0;
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_srl_epi16) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_srl_epi16) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_srl_epi16) (mask, src1.x, src2.x);

  CALC (res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();

  src2.a[0] = 17;
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_srl_epi16) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_srl_epi16) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_srl_epi16) (mask, src1.x, src2.x);

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
