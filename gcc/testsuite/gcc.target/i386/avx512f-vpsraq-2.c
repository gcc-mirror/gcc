/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

CALC (long long *r, long long *s1, long long *s2)
{
  int i;
  long long count = s2[0];
  for (i = 0; i < SIZE; i++)
    {
      r[i] =
	count < 64 ? (s1[i] >> count) : (s1[i] >
					 0 ? 0 : 0xFFFFFFFFFFFFFFFFLL);
    }
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3, src1;
  UNION_TYPE (128, i_q) src2;
  MASK_TYPE mask = MASK_VALUE;
  long long res_ref[SIZE];

  long long imm;
  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + sign * 7 * i % 291;
      sign = sign * -1;
    }

  for (imm = 1; imm <= 65; imm++)
    {
      src2.a[0] = imm;
      for (i = 0; i < SIZE; i++)
	res2.a[i] = DEFAULT_VALUE;

      res1.x = INTRINSIC (_sra_epi64) (src1.x, src2.x);
      res2.x =
	INTRINSIC (_mask_sra_epi64) (res2.x, mask, src1.x, src2.x);
      res3.x = INTRINSIC (_maskz_sra_epi64) (mask, src1.x, src2.x);

      CALC (res_ref, src1.a, src2.a);

      if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
	abort ();

      MASK_MERGE (i_q) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
	abort ();

      MASK_ZERO (i_q) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
	abort ();
    }
}
