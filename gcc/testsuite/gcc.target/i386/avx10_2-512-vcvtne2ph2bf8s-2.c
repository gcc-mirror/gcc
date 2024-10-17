/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif

#include "avx10-helper.h"
#include "fp8-helper.h"

#define SIZE_SRC (AVX512F_LEN / 16)
#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

void
CALC (unsigned char *r, _Float16 *s1, _Float16 *s2)
{
  _Float16 temp;
  Float16Union ut = {.f16 = temp};
  int i, hf8_bf8, saturate;

  hf8_bf8 = 1;
  saturate = 1;
  
  for (i = 0; i < SIZE; i++)
    {
      r[i] = 0;
      if (i < SIZE_SRC)
        {
          Float16Union usrc2 = {.f16 = s2[i]};
          ut.u16 = usrc2.u16;
        }
      else
        {
          Float16Union usrc1 = {.f16 = s1[i-SIZE_SRC]};
          ut.u16 = usrc1.u16;
        }
      r[i] = convert_fp16_to_fp8(ut.f16, 0, hf8_bf8, saturate);
    }
}

void
TEST (void)
{
  int i,sign;
  UNION_TYPE (AVX512F_LEN, i_b) res1, res2, res3; 
  UNION_TYPE (AVX512F_LEN, h) src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned char res_ref[SIZE];

  sign = 1;
  for (i = 0; i < SIZE_SRC; i++)
    {
      src1.a[i] = (_Float16)(sign * (1.5 * (1 << (i % 3))));
      src2.a[i] = (_Float16)(-sign * (2.5 * (1 << (i % 3))));
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  CALC(res_ref, src1.a, src2.a);

  res1.x = INTRINSIC (_cvtnes2ph_pbf8) (src1.x, src2.x);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  res2.x = INTRINSIC (_mask_cvtnes2ph_pbf8) (res2.x, mask, src1.x, src2.x);
  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  res3.x = INTRINSIC (_maskz_cvtnes2ph_pbf8) (mask, src1.x, src2.x);
  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();
}
