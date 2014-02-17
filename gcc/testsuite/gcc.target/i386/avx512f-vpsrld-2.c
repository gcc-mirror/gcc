/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

CALC (unsigned int *r, unsigned int *s1,  unsigned long long* s2)
{
  int i;
  unsigned int count = s2[0];
  for (i = 0; i < SIZE; i++)
    {
      r[i] = count < 32 ? (s1[i] >> count) : 0;
    }
}

void static
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3, src1;
  UNION_TYPE (128, i_q) src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned int res_ref[SIZE];

  unsigned long long imm;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + 7 * i % 291;
    }

  for (imm = 1; imm <= 33; imm++)
    {
      src2.a[0] = imm;
      for (i = 0; i < SIZE; i++)
	res2.a[i] = DEFAULT_VALUE;

      res1.x = INTRINSIC (_srl_epi32) (src1.x, src2.x);
      res2.x = INTRINSIC (_mask_srl_epi32) (res2.x, mask, src1.x, src2.x);
      res3.x = INTRINSIC (_maskz_srl_epi32) (mask, src1.x, src2.x);

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
