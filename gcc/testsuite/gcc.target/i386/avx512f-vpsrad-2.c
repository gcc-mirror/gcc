/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

CALC (int *r, int *s1, int *s2)
{
  int i;
  int count = s2[0];
  for (i = 0; i < SIZE; i++)
    {
      r[i] =
	count < 32 ? (s1[i] >> count) : (s1[i] > 0 ? 0 : 0xFFFFFFFF);
    }
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3, src1;
  UNION_TYPE (128, i_d) src2;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];

  long long imm;
  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + sign * 7 * i % 291;
      sign = sign * -1;
    }

  for (imm = 1; imm <= 33; imm++)
    {
      src2.a[0] = imm;
      for (i = 0; i < SIZE; i++)
	res2.a[i] = DEFAULT_VALUE;

      res1.x = INTRINSIC (_sra_epi32) (src1.x, src2.x);
      res2.x =
	INTRINSIC (_mask_sra_epi32) (res2.x, mask, src1.x, src2.x);
      res3.x = INTRINSIC (_maskz_sra_epi32) (mask, src1.x, src2.x);

      CALC (res_ref, src1.a, src2.a);

      if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
	abort ();

      MASK_MERGE (i_d) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
	abort ();

      MASK_ZERO (i_d) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
	abort ();
    }
}
