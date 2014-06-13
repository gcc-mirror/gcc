/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (int *src1, int *src2, int *src3, int imm, int *r)
{
  int i, j, index, res, mask, one_mask = 1;
  int src1_bit, src2_bit, src3_bit, imm_bit;

  for (i = 0; i < SIZE; i++)
    {
      res = 0;
      for (j = 0; j < 32; j++)
	{
	  mask = one_mask << j;
	  src1_bit = ((src1[i] & mask) >> j) << 2;
	  src2_bit = ((src2[i] & mask) >> j) << 1;
	  src3_bit = ((src3[i] & mask) >> j);
	  index = src1_bit | src2_bit | src3_bit;
	  imm_bit = (imm & (one_mask << index)) >> index;
	  res = res | (imm_bit << j);
	}
      r[i] = res;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) src2, src3, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];
  int i, imm = 0x7D;

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      src2.a[i] = 145132 * i + 123123;
      src3.a[i] = 1223 * i + 895;
    }

  CALC (res1.a, src2.a, src3.a, imm, res_ref);

  res1.x = INTRINSIC (_ternarylogic_epi32) (res1.x, src2.x, src3.x,
    imm);
  res2.x = INTRINSIC (_mask_ternarylogic_epi32) (res2.x, mask, src2.x,
    src3.x, imm);
  res3.x = INTRINSIC (_maskz_ternarylogic_epi32) (mask, res3.x, src2.x,
    src3.x, imm);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
