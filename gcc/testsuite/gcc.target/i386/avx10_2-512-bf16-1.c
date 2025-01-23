/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-512 -O2" } */
/* { dg-final { scan-assembler-times "vaddbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vaddbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vaddbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsubbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsubbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsubbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmulbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmulbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmulbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdivbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdivbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdivbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmaxbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmaxbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmaxbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vminbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vminbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vminbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefpbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefpbf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmadd231bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub231bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd231bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub231bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrtpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrtpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrtpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsqrtnepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsqrtnepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsqrtnepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrcpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrcpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrcpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetexppbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetexppbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetexppbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalenepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrndscalenepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalenepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreducenepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vreducenepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreducenepbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vgetmantpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantpbf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfpclasspbf16z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfpclasspbf16z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpbf16\[ \\t\]+\\\$1\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%k\[0-9\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpbf16\[ \\t\]+\\\$2\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%k\[0-9\]\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

#define IMM 123

volatile __m512bh res, x1, x2;
volatile __mmask32 m32;

void extern
avx10_2_512_test (void)
{
  res = _mm512_add_pbh (x1, x2);
  res = _mm512_mask_add_pbh (res, m32, x1, x2);
  res = _mm512_maskz_add_pbh (m32, x1, x2);
  res = _mm512_sub_pbh (x1, x2);
  res = _mm512_mask_sub_pbh (res, m32, x1, x2);
  res = _mm512_maskz_sub_pbh (m32, x1, x2);
  res = _mm512_mul_pbh (x1, x2);
  res = _mm512_mask_mul_pbh (res, m32, x1, x2);
  res = _mm512_maskz_mul_pbh (m32, x1, x2);
  res = _mm512_div_pbh (x1, x2);
  res = _mm512_mask_div_pbh (res, m32, x1, x2);
  res = _mm512_maskz_div_pbh (m32, x1, x2);
  res = _mm512_max_pbh (x1, x2);
  res = _mm512_mask_max_pbh (res, m32, x1, x2);
  res = _mm512_maskz_max_pbh (m32, x1, x2);
  res = _mm512_min_pbh (x1, x2);
  res = _mm512_mask_min_pbh (res, m32, x1, x2);
  res = _mm512_maskz_min_pbh (m32, x1, x2);
  res = _mm512_scalef_pbh (x1, x2);
  res = _mm512_mask_scalef_pbh (res, m32, x1, x2);
  res = _mm512_maskz_scalef_pbh (m32, x1, x2);

  res = _mm512_fmadd_pbh (res, x1, x2);
  res = _mm512_mask_fmadd_pbh (res, m32, x1, x2);
  res = _mm512_mask3_fmadd_pbh (res, x1, x2, m32);
  res = _mm512_maskz_fmadd_pbh (m32,res, x1, x2);
  res = _mm512_fmsub_pbh (res, x1, x2);
  res = _mm512_mask_fmsub_pbh (res, m32, x1, x2);
  res = _mm512_mask3_fmsub_pbh (res, x1, x2, m32);
  res = _mm512_maskz_fmsub_pbh (m32,res, x1, x2);
  res = _mm512_fnmadd_pbh (res, x1, x2);
  res = _mm512_mask_fnmadd_pbh (res, m32, x1, x2);
  res = _mm512_mask3_fnmadd_pbh (res, x1, x2, m32);
  res = _mm512_maskz_fnmadd_pbh (m32,res, x1, x2);
  res = _mm512_fnmsub_pbh (res, x1, x2);
  res = _mm512_mask_fnmsub_pbh (res, m32, x1, x2);
  res = _mm512_mask3_fnmsub_pbh (res, x1, x2, m32);
  res = _mm512_maskz_fnmsub_pbh (m32,res, x1, x2);
  
  res = _mm512_rsqrt_pbh (x1);
  res = _mm512_mask_rsqrt_pbh (res, m32, x1);
  res = _mm512_maskz_rsqrt_pbh (m32, x1);
  res = _mm512_sqrtne_pbh (x1);
  res = _mm512_mask_sqrtne_pbh (res, m32, x1);
  res = _mm512_maskz_sqrtne_pbh (m32, x1);
  res = _mm512_rcp_pbh (x1);
  res = _mm512_mask_rcp_pbh (res, m32, x1);
  res = _mm512_maskz_rcp_pbh (m32, x1);
  res = _mm512_getexp_pbh (x1);
  res = _mm512_mask_getexp_pbh (res, m32, x1);
  res = _mm512_maskz_getexp_pbh (m32, x1);
  
  res = _mm512_roundscalene_pbh (x1, IMM);
  res = _mm512_mask_roundscalene_pbh (res, m32, x1, IMM);
  res = _mm512_maskz_roundscalene_pbh (m32, x1, IMM);
  res = _mm512_reducene_pbh (x1, IMM);
  res = _mm512_mask_reducene_pbh (res, m32, x1, IMM);
  res = _mm512_maskz_reducene_pbh (m32, x1, IMM);
  res = _mm512_getmant_pbh (x1, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  res = _mm512_mask_getmant_pbh (res, m32, x1, _MM_MANT_NORM_p75_1p5,
				 _MM_MANT_SIGN_src);
  res = _mm512_maskz_getmant_pbh (m32, x1, _MM_MANT_NORM_p75_1p5,
				  _MM_MANT_SIGN_src);

  m32 = _mm512_fpclass_pbh_mask (x1, 13);
  m32 = _mm512_mask_fpclass_pbh_mask (2, x1, 13);
  
  m32 = _mm512_cmp_pbh_mask (x1, x2, 1);
  m32 = _mm512_mask_cmp_pbh_mask (m32, x1, x2, 2);
}
