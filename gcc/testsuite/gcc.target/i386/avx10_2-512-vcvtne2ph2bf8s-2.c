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

#define SIZE_SRC (AVX512F_LEN / 16)
#define SIZE_RES (AVX512F_LEN / 8)

void
CALC (unsigned char *r, _Float16 *s1, _Float16 *s2)
{
  _Float16 temp;
  Float16Union ut = {.f16 = temp};
  int i, hf8_bf8, saturate;

  hf8_bf8 = 1;
  saturate = 1;
  
  for (i = 0; i < SIZE_RES; i++)
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
  UNION_TYPE (AVX512F_LEN, i_b) res; 
  UNION_TYPE (AVX512F_LEN, h) src1, src2;
  unsigned char res_ref[SIZE_RES];

  sign = 1;
  for (i = 0; i < SIZE_SRC; i++)
    {
      src1.a[i] = (_Float16)(sign * (1.5 * (1 << (i % 3))));
      src2.a[i] = (_Float16)(-sign * (2.5 * (1 << (i % 3))));
      sign = -sign;
    }

  res.x = INTRINSIC (_cvtnes2ph_pbf8) (src1.x, src2.x);
  CALC(res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res, res_ref))
    abort ();
}
