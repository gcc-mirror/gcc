/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *s, unsigned char imm, short *r)
{
  int i;

  for (i = 0; i < SIZE / 8; i++)
    {
      r[8 * i] = s[8 * i];
      r[8 * i + 1] = s[8 * i + 1];
      r[8 * i + 2] = s[8 * i + 2];
      r[8 * i + 3] = s[8 * i + 3];
      r[8 * i + 4] = s[8 * i + (imm >> 0 & 3) + 4];
      r[8 * i + 5] = s[8 * i + (imm >> 2 & 3) + 4];
      r[8 * i + 6] = s[8 * i + (imm >> 4 & 3) + 4];
      r[8 * i + 7] = s[8 * i + (imm >> 6 & 3) + 4];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, res1, res2, res3;
  short res_ref[SIZE];
  int i, sign = 1;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * i * sign;
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_shufflehi_epi16) (s1.x, 0xec);
  res2.x =
    INTRINSIC (_mask_shufflehi_epi16) (res2.x, mask, s1.x, 0xec);
  res3.x = INTRINSIC (_maskz_shufflehi_epi16) (mask, s1.x, 0xec);

  CALC (s1.a, 0xec, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
