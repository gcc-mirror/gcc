/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *r, short *s1, short *s2, MASK_TYPE mask)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = (mask & (1 << i)) ? s2[i] : s1[i];
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_w) res1, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 15 + 3467 * i * sign;
      src2.a[i] = -2217 * i * sign;
      sign = sign * -1;
    }

  res1.x = INTRINSIC (_mask_blend_epi16) (mask, src1.x, src2.x);

  CALC (res_ref, src1.a, src2.a, mask);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();
}
