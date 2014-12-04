/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE    (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *s1, short *s2, short *r)
{
  int i;

  for (i = 0; i < SIZE; ++i)
    {
      r[i] = ((unsigned short) s1[i]) << s2[i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, s2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * sign;
      s2.a[i] = i >> 2;
      res2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_sllv_epi16) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_sllv_epi16) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_sllv_epi16) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
