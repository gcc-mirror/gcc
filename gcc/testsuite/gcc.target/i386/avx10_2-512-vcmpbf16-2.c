/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
TEST (void)
{
  int i;
  __mmask32 res1, res2, exp = 0;
  UNION_TYPE (AVX512F_LEN, bf16_uw) src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  
  for (i = 0; i < SIZE; i++)
    {
      float x = 0.5;
      float y = 0.25;
      src2.a[i] = convert_fp32_to_bf16 (y);
      src1.a[i] = convert_fp32_to_bf16 (x);
      if (src1.a[i] == src2.a[i])
	exp |= 1 << i;
    }

  res1 = INTRINSIC (_cmp_pbh_mask) (src1.x, src2.x, 0);
  res2 = INTRINSIC (_mask_cmp_pbh_mask) (mask, src1.x, src2.x, 0);

  if (exp != res1 || exp != res2)
    abort ();
}
