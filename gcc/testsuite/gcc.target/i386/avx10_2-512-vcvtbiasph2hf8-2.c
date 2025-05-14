/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif

#include "avx10-helper.h"
#include "fp8-helper.h"

#define SRC_F8_I8 (AVX512F_LEN / 8)
#define SRC_F16 (AVX512F_LEN / 16)
#define DST_F8_I8 (AVX512F_LEN_HALF / 8)
#define DST_F16 (AVX512F_LEN_HALF / 16)
#define SIZE SRC_F16 

#include "avx512f-mask-type.h"

void
CALC (unsigned char *r, char *src1, _Float16 *src2)
{
  int i, hf8_bf8, saturate;

  hf8_bf8 = 0;
  saturate = 0;
  
  for (i = 0; i < DST_F8_I8; i++)
    {
      Float16Union usrc = {.f16 = src2[i]};
      r[i] = convert_fp16_to_fp8(usrc.f16, src1[2 * i], hf8_bf8, saturate);
    }

  if (AVX512F_LEN == 128)
    for (i = DST_F16; i < DST_F8_I8; i++)
      r[i] = 0;
}

void
TEST (void)
{
  int i,sign;
  UNION_TYPE (AVX512F_LEN_HALF, i_b) res1, res2, res3; 
  UNION_TYPE (AVX512F_LEN, i_b) src1;
  UNION_TYPE (AVX512F_LEN, h) src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned char res_ref[DST_F8_I8];

  sign = 1;
  for (i = 0; i < SRC_F16; i++)
    {
      src2.a[i] = (_Float16)(sign * (2.5 * (1 << (i % 3))));
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  CALC (res_ref, src1.a, src2.a);

  res1.x = INTRINSIC (_cvtbiasph_hf8) (src1.x, src2.x);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res1, res_ref))
    abort ();

  res2.x = INTRINSIC (_mask_cvtbiasph_hf8) (res2.x, mask, src1.x, src2.x);
  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res2, res_ref))
    abort ();

  res3.x = INTRINSIC (_maskz_cvtbiasph_hf8) (mask, src1.x, src2.x);
  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res3, res_ref))
    abort ();
}
