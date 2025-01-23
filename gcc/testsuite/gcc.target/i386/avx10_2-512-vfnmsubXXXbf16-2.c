/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

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
  UNION_TYPE (AVX512F_LEN, bf16_uw) res1, res2, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[SIZE], res_ref2[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      float x = 0.5;
      float y = 2;
      float z = 0.25;
      src1.a[i] = convert_fp32_to_bf16 (x);
      src2.a[i] = convert_fp32_to_bf16 (y);
      res1.a[i] = convert_fp32_to_bf16 (z);
      res2.a[i] = res1.a[i];
      float x16, y16, z16, m1, m2;
      x16 = convert_bf16_to_fp32 (src1.a[i]);
      y16 = convert_bf16_to_fp32 (src2.a[i]);
      z16 = convert_bf16_to_fp32 (res1.a[i]);
      m1 = -y16 - x16 * z16;
      m2 = -z16 - x16 * y16;
      res_ref[i] = convert_fp32_to_bf16 (m1);
      res_ref2[i] = convert_fp32_to_bf16 (m2);
    }

  MASK_MERGE (bf16_uw) (res1.a, mask, SIZE);
  MASK_MERGE (bf16_uw) (res2.a, mask, SIZE);
  res1.x = INTRINSIC (_mask_fnmsub_pbh) (res1.x, mask, src1.x, src2.x);
  res2.x = INTRINSIC (_mask3_fnmsub_pbh) (src1.x, src2.x, res2.x, mask);
  
  MASK_MERGE (bf16_uw) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, bf16_uw) (res1, res_ref))
    abort ();
  
  MASK_MERGE (bf16_uw) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, bf16_uw) (res2, res_ref2))
    abort ();
}
