/* { dg-do run } */
/* { dg-options "-O2 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

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
  UNION_TYPE (AVX512F_LEN_HALF, i_b) res; 
  UNION_TYPE (AVX512F_LEN, i_b) src1;
  UNION_TYPE (AVX512F_LEN, h) src2;
  unsigned char res_ref[DST_F8_I8];

  sign = 1;
  for (i = 0; i < SRC_F16; i++)
    {
      src2.a[i] = (_Float16)(sign * (2.5 * (1 << (i % 3))));
      sign = -sign;
    }

  res.x = INTRINSIC (_cvtbiasph_phf8) (src1.x, src2.x);
  CALC(res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res, res_ref))
    abort ();
}
