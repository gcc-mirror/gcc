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
  MASK_TYPE res1 = 0, res2 = 0;
  __mmask16 exp = 0;
  UNION_TYPE (AVX512F_LEN, bf16_uw) src1;
  UNION_TYPE (AVX512F_LEN, ) src2;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE / 2; i++)
    {
      src1.a[i] = 0;
      src2.a[i] = (uint32_t) (src1.a[i]) << 16;
    }

  for (i = SIZE / 2; i < SIZE; i++)
    src1.a[i] = 0;
  
  src1.a[0] = 0x7FC0;
  src2.a[0] = convert_bf16_to_fp32 (src1.a[0]);
  
  _mm_setcsr (0x9FC0);
  exp = INTRINSIC (_fpclass_ps_mask) (src2.x, 0x01);
  
  _mm_setcsr (0x1f80);
  res1 = INTRINSIC (_fpclass_pbh_mask) (src1.x, 0x01);
  res2 = INTRINSIC (_mask_fpclass_pbh_mask) (mask, src1.x, 1);

  if (exp != res1 || (mask & exp) != res2)
    abort ();
}
