/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#include <math.h>
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, bf16_uw) res1, res2, res3, src1;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[SIZE], res_ref2[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = 0;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      float s1 = i + 1.0;
      float rs = sqrtf (s1);
      src1.a[i] = convert_fp32_to_bf16_ne (s1);
      res_ref[i] = res_ref2[i] =  convert_fp32_to_bf16_ne (rs);
    }

  res1.x = INTRINSIC (_sqrt_pbh) (src1.x);
  res2.x = INTRINSIC (_mask_sqrt_pbh) (res2.x, mask, src1.x);
  res3.x = INTRINSIC (_maskz_sqrt_pbh) (mask, src1.x);

  if (UNION_CHECK (AVX512F_LEN, bf16_uw) (res1, res_ref))
    abort ();
  
  MASK_MERGE (bf16_uw) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, bf16_uw) (res2, res_ref2))
    abort ();

  MASK_ZERO (bf16_uw) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, bf16_uw) (res3, res_ref2))
    abort ();
}
