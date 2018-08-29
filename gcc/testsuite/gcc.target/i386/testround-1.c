/* { dg-do compile } */
/* { dg-options "-O0 -mavx512f" } */

#include <x86intrin.h>

int i;
unsigned int ui;
__m512i m512i;
__m512d m512d;
__m512  m512;
__m256i m256i;
__m256  m256;
__m128i m128i;
__m128d m128d;
__m128  m128;
__mmask8 mmask8;
__mmask16 mmask16;

void
test_round (void)
{
  m128d = _mm_add_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_add_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_add_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_add_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_add_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_add_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_sub_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_sub_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_sub_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_sub_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_sub_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_sub_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_sqrt_round_pd (m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_sqrt_round_pd (m512d, mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_sqrt_round_pd (mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_sqrt_round_ps (m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_sqrt_round_ps (m512, mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_sqrt_round_ps (mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_sqrt_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_sqrt_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_add_round_pd (m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_add_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_add_round_pd (mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_add_round_ps (m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_add_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_add_round_ps (mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_sub_round_pd (m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_sub_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_sub_round_pd (mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_sub_round_ps (m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_sub_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_sub_round_ps (mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_mul_round_pd (m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_mul_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_mul_round_pd (mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mul_round_ps (m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_mul_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_mul_round_ps (mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_div_round_pd (m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_div_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_div_round_pd (mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_div_round_ps (m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_div_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_div_round_ps (mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mul_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_mul_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_mul_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mul_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_mul_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_mul_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_div_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_div_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_div_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_div_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_div_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_div_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_scalef_round_pd(m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_scalef_round_pd(m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_scalef_round_pd(mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_scalef_round_ps(m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_scalef_round_ps(m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_scalef_round_ps(mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_scalef_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_scalef_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_fmadd_round_pd (m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmadd_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmadd_round_pd (m512d, m512d, m512d, mmask8, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmadd_round_pd (mmask8, m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmadd_round_ps (m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmadd_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmadd_round_ps (m512, m512, m512, mmask16, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmadd_round_ps (mmask16, m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fmsub_round_pd (m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmsub_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmsub_round_pd (m512d, m512d, m512d, mmask8, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmsub_round_pd (mmask8, m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmsub_round_ps (m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmsub_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmsub_round_ps (m512, m512, m512, mmask16, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmsub_round_ps (mmask16, m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fmaddsub_round_pd (m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmaddsub_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmaddsub_round_pd (m512d, m512d, m512d, mmask8, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmaddsub_round_pd (mmask8, m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmaddsub_round_ps (m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmaddsub_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmaddsub_round_ps (m512, m512, m512, mmask16, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmaddsub_round_ps (mmask16, m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fmsubadd_round_pd (m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmsubadd_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmsubadd_round_pd (m512d, m512d, m512d, mmask8, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmsubadd_round_pd (mmask8, m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmsubadd_round_ps (m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmsubadd_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmsubadd_round_ps (m512, m512, m512, mmask16, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmsubadd_round_ps (mmask16, m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fnmadd_round_pd (m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fnmadd_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fnmadd_round_pd (m512d, m512d, m512d, mmask8, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fnmadd_round_pd (mmask8, m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fnmadd_round_ps (m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fnmadd_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fnmadd_round_ps (m512, m512, m512, mmask16, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fnmadd_round_ps (mmask16, m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fnmsub_round_pd (m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fnmsub_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fnmsub_round_pd (m512d, m512d, m512d, mmask8, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fnmsub_round_pd (mmask8, m512d, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fnmsub_round_ps (m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fnmsub_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fnmsub_round_ps (m512, m512, m512, mmask16, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fnmsub_round_ps (mmask16, m512, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */

  m256i = _mm512_cvt_roundpd_epi32 (m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvt_roundpd_epi32 (m256i, mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvt_roundpd_epi32 (mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_cvt_roundpd_epu32 (m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvt_roundpd_epu32 (m256i, mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvt_roundpd_epu32 (mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */

  m512i = _mm512_cvt_roundps_epi32 (m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvt_roundps_epi32 (m512i, mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvt_roundps_epi32 (mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_cvt_roundps_epu32 (m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvt_roundps_epu32 (m512i, mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvt_roundps_epu32 (mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */

  m128 = _mm_cvt_roundu32_ss (m128, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_cvt_roundi32_ss (m128, 4, 7); /* { dg-error "incorrect rounding operand" } */

  m512 = _mm512_cvt_roundepi32_ps (m512i, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_cvt_roundepi32_ps (m512, mmask16, m512i, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_cvt_roundepi32_ps (mmask16, m512i, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_cvt_roundepu32_ps (m512i, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_cvt_roundepu32_ps (m512, mmask16, m512i, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_cvt_roundepu32_ps (mmask16, m512i, 7); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvt_roundss_u32 (m128, 7); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvt_roundss_i32 (m128, 7); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvt_roundsd_u32 (m128d, 7); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvt_roundsd_i32 (m128d, 7); /* { dg-error "incorrect rounding operand" } */

  m256 = _mm512_cvt_roundpd_ps (m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256 = _mm512_mask_cvt_roundpd_ps (m256, mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256 = _mm512_maskz_cvt_roundpd_ps (mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_cvt_roundsd_ss (m128, m128d, 7); /* { dg-error "incorrect rounding operand" } */

  m128d = _mm_fmadd_round_sd (m128d, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fmadd_round_ss (m128, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fmsub_round_sd (m128d, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fmsub_round_ss (m128, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fnmadd_round_sd (m128d, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fnmadd_round_ss (m128, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fnmsub_round_sd (m128d, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fnmsub_round_ss (m128, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_max_round_pd (m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_max_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_max_round_pd (mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_max_round_ps (m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_max_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_max_round_ps (mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_max_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_max_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_max_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_max_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_max_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_max_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_min_round_pd (m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_min_round_pd (m512d, mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_min_round_pd (mmask8, m512d, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_min_round_ps (m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_min_round_ps (m512, mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_min_round_ps (mmask16, m512, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_min_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_min_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_min_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_min_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_min_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_min_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m256i = _mm512_cvtt_roundpd_epi32 (m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvtt_roundpd_epi32 (m256i, mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvtt_roundpd_epi32 (mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_cvtt_roundpd_epu32 (m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvtt_roundpd_epu32 (m256i, mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvtt_roundpd_epu32 (mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */

  m512i = _mm512_cvtt_roundps_epi32 (m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvtt_roundps_epi32 (m512i, mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvtt_roundps_epi32 (mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_cvtt_roundps_epu32 (m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvtt_roundps_epu32 (m512i, mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvtt_roundps_epu32 (mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_fixupimm_round_pd (m512d, m512d, m512i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fixupimm_round_pd (m512d, mmask8, m512d, m512i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fixupimm_round_pd (mmask8, m512d, m512d, m512i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fixupimm_round_ps (m512, m512, m512i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fixupimm_round_ps (m512, mmask16, m512, m512i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fixupimm_round_ps (mmask16, m512, m512, m512i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fixupimm_round_sd (m128d, m128d, m128i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_fixupimm_round_sd (m128d, mmask8, m128d, m128i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_fixupimm_round_sd (mmask8, m128d, m128d, m128i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fixupimm_round_ss (m128, m128, m128i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_fixupimm_round_ss (m128, mmask8, m128, m128i, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_fixupimm_round_ss (mmask8, m128, m128, m128i, 4, 7); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvtt_roundss_u32 (m128, 7); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvtt_roundss_i32 (m128, 7); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvtt_roundsd_u32 (m128d, 7); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvtt_roundsd_i32 (m128d, 7); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_cvt_roundps_pd (m256, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_cvt_roundps_pd (m512d, mmask8, m256, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_cvt_roundps_pd (mmask8, m256, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_cvt_roundph_ps (m256i, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_cvt_roundph_ps (m512, mmask16, m256i, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_cvt_roundph_ps (mmask16, m256i, 7); /* { dg-error "incorrect rounding operand" } */

  m128d = _mm_cvt_roundss_sd (m128d, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m128 = _mm_getexp_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_getexp_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_getexp_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_getexp_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_getexp_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_getexp_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_getexp_round_ps (m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_getexp_round_ps (m512, mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_getexp_round_ps (mmask16, m512, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_getexp_round_pd (m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_getexp_round_pd (m512d, mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_getexp_round_pd (mmask8, m512d, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_getmant_round_pd (m512d, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_getmant_round_pd (m512d, mmask8, m512d, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_getmant_round_pd (mmask8, m512d, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_getmant_round_ps (m512, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_getmant_round_ps (m512, mmask16, m512, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_getmant_round_ps (mmask16, m512, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_getmant_round_sd (m128d, m128d, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_getmant_round_sd (m128d, mmask8, m128d, m128d, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_getmant_round_sd (mmask8, m128d, m128d, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_getmant_round_ss (m128, m128, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_getmant_round_ss (m128, mmask8, m128, m128, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_getmant_round_ss (mmask8, m128, m128, 0, 0, 7); /* { dg-error "incorrect rounding operand" } */

  m512 = _mm512_roundscale_round_ps (m512, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_roundscale_round_ps (m512, mmask16, m512, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_roundscale_round_ps (mmask16, m512, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_roundscale_round_pd (m512d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_roundscale_round_pd (m512d, mmask8, m512d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_roundscale_round_pd (mmask8, m512d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_roundscale_round_ss (m128, m128, 4, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_roundscale_round_sd (m128d, m128d, 4, 7); /* { dg-error "incorrect rounding operand" } */

  mmask8 = _mm512_cmp_round_pd_mask (m512d, m512d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm512_cmp_round_ps_mask (m512, m512, 4, 7); /* { dg-error "incorrect rounding operand" } */
  mmask8 = _mm512_mask_cmp_round_pd_mask (mmask8, m512d, m512d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm512_mask_cmp_round_ps_mask (mmask16, m512, m512, 4, 7); /* { dg-error "incorrect rounding operand" } */
  mmask8 = _mm_cmp_round_sd_mask (m128d, m128d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  mmask8 = _mm_mask_cmp_round_sd_mask (mmask8, m128d, m128d, 4, 7); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm_cmp_round_ss_mask (m128, m128, 4, 7); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm_mask_cmp_round_ss_mask (mmask8, m128, m128, 4, 7); /* { dg-error "incorrect rounding operand" } */

  i = _mm_comi_round_ss (m128, m128, 4, 7); /* { dg-error "incorrect rounding operand" } */
  i = _mm_comi_round_sd (m128d, m128d, 4, 7); /* { dg-error "incorrect rounding operand" } */
}

void
test_round_sae (void)
{
  m128d = _mm_add_round_sd (m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_add_round_sd (m128d, mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_add_round_sd (mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_add_round_ss (m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_add_round_ss (m128, mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_add_round_ss (mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_sub_round_sd (m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_sub_round_sd (m128d, mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_sub_round_sd (mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_sub_round_ss (m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_sub_round_ss (m128, mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_sub_round_ss (mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_sqrt_round_pd (m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_sqrt_round_pd (m512d, mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_sqrt_round_pd (mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_sqrt_round_ps (m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_sqrt_round_ps (m512, mmask16, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_sqrt_round_ps (mmask16, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_sqrt_round_sd (m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_sqrt_round_ss (m128, m128, 5); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_add_round_pd (m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_add_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_add_round_pd (mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_add_round_ps (m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_add_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_add_round_ps (mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_sub_round_pd (m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_sub_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_sub_round_pd (mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_sub_round_ps (m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_sub_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_sub_round_ps (mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_mul_round_pd (m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_mul_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_mul_round_pd (mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mul_round_ps (m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_mul_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_mul_round_ps (mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_div_round_pd (m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_div_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_div_round_pd (mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_div_round_ps (m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_div_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_div_round_ps (mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mul_round_sd (m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_mul_round_sd (m128d, mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_mul_round_sd (mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mul_round_ss (m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_mul_round_ss (m128, mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_mul_round_ss (mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_div_round_sd (m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_div_round_sd (m128d, mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_div_round_sd (mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_div_round_ss (m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_div_round_ss (m128, mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_div_round_ss (mmask8, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_scalef_round_pd(m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_scalef_round_pd(m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_scalef_round_pd(mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_scalef_round_ps(m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_scalef_round_ps(m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_scalef_round_ps(mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_scalef_round_sd (m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_scalef_round_ss (m128, m128, 5); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_fmadd_round_pd (m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmadd_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmadd_round_pd (m512d, m512d, m512d, mmask8, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmadd_round_pd (mmask8, m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmadd_round_ps (m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmadd_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmadd_round_ps (m512, m512, m512, mmask16, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmadd_round_ps (mmask16, m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fmsub_round_pd (m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmsub_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmsub_round_pd (m512d, m512d, m512d, mmask8, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmsub_round_pd (mmask8, m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmsub_round_ps (m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmsub_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmsub_round_ps (m512, m512, m512, mmask16, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmsub_round_ps (mmask16, m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fmaddsub_round_pd (m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmaddsub_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmaddsub_round_pd (m512d, m512d, m512d, mmask8, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmaddsub_round_pd (mmask8, m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmaddsub_round_ps (m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmaddsub_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmaddsub_round_ps (m512, m512, m512, mmask16, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmaddsub_round_ps (mmask16, m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fmsubadd_round_pd (m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fmsubadd_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fmsubadd_round_pd (m512d, m512d, m512d, mmask8, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fmsubadd_round_pd (mmask8, m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fmsubadd_round_ps (m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fmsubadd_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fmsubadd_round_ps (m512, m512, m512, mmask16, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fmsubadd_round_ps (mmask16, m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fnmadd_round_pd (m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fnmadd_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fnmadd_round_pd (m512d, m512d, m512d, mmask8, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fnmadd_round_pd (mmask8, m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fnmadd_round_ps (m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fnmadd_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fnmadd_round_ps (m512, m512, m512, mmask16, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fnmadd_round_ps (mmask16, m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_fnmsub_round_pd (m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fnmsub_round_pd (m512d, mmask8, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask3_fnmsub_round_pd (m512d, m512d, m512d, mmask8, 5); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fnmsub_round_pd (mmask8, m512d, m512d, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fnmsub_round_ps (m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fnmsub_round_ps (m512, mmask16, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask3_fnmsub_round_ps (m512, m512, m512, mmask16, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fnmsub_round_ps (mmask16, m512, m512, m512, 5); /* { dg-error "incorrect rounding operand" } */

  m256i = _mm512_cvt_roundpd_epi32 (m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvt_roundpd_epi32 (m256i, mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvt_roundpd_epi32 (mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_cvt_roundpd_epu32 (m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvt_roundpd_epu32 (m256i, mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvt_roundpd_epu32 (mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */

  m512i = _mm512_cvt_roundps_epi32 (m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvt_roundps_epi32 (m512i, mmask16, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvt_roundps_epi32 (mmask16, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_cvt_roundps_epu32 (m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvt_roundps_epu32 (m512i, mmask16, m512, 5); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvt_roundps_epu32 (mmask16, m512, 5); /* { dg-error "incorrect rounding operand" } */

  m128 = _mm_cvt_roundu32_ss (m128, 4, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_cvt_roundi32_ss (m128, 4, 5); /* { dg-error "incorrect rounding operand" } */

  m512 = _mm512_cvt_roundepi32_ps (m512i, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_cvt_roundepi32_ps (m512, mmask16, m512i, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_cvt_roundepi32_ps (mmask16, m512i, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_cvt_roundepu32_ps (m512i, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_cvt_roundepu32_ps (m512, mmask16, m512i, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_cvt_roundepu32_ps (mmask16, m512i, 5); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvt_roundss_u32 (m128, 5); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvt_roundss_i32 (m128, 5); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvt_roundsd_u32 (m128d, 5); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvt_roundsd_i32 (m128d, 5); /* { dg-error "incorrect rounding operand" } */

  m256 = _mm512_cvt_roundpd_ps (m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m256 = _mm512_mask_cvt_roundpd_ps (m256, mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m256 = _mm512_maskz_cvt_roundpd_ps (mmask8, m512d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_cvt_roundsd_ss (m128, m128d, 5); /* { dg-error "incorrect rounding operand" } */

  m128d = _mm_fmadd_round_sd (m128d, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fmadd_round_ss (m128, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fmsub_round_sd (m128d, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fmsub_round_ss (m128, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fnmadd_round_sd (m128d, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fnmadd_round_ss (m128, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fnmsub_round_sd (m128d, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fnmsub_round_ss (m128, m128, m128, 5); /* { dg-error "incorrect rounding operand" } */
}

void
test_sae_only (void)
{
  m512d = _mm512_max_round_pd (m512d, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_max_round_pd (m512d, mmask8, m512d, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_max_round_pd (mmask8, m512d, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_max_round_ps (m512, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_max_round_ps (m512, mmask16, m512, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_max_round_ps (mmask16, m512, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_max_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_max_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_max_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_max_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_max_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_max_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_min_round_pd (m512d, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_min_round_pd (m512d, mmask8, m512d, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_min_round_pd (mmask8, m512d, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_min_round_ps (m512, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_min_round_ps (m512, mmask16, m512, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_min_round_ps (mmask16, m512, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_min_round_sd (m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_min_round_sd (m128d, mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_min_round_sd (mmask8, m128d, m128d, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_min_round_ss (m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_min_round_ss (m128, mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_min_round_ss (mmask8, m128, m128, 7); /* { dg-error "incorrect rounding operand" } */

  m256i = _mm512_cvtt_roundpd_epi32 (m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvtt_roundpd_epi32 (m256i, mmask8, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvtt_roundpd_epi32 (mmask8, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_cvtt_roundpd_epu32 (m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_mask_cvtt_roundpd_epu32 (m256i, mmask8, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m256i = _mm512_maskz_cvtt_roundpd_epu32 (mmask8, m512d, 3); /* { dg-error "incorrect rounding operand" } */

  m512i = _mm512_cvtt_roundps_epi32 (m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvtt_roundps_epi32 (m512i, mmask16, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvtt_roundps_epi32 (mmask16, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_cvtt_roundps_epu32 (m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_mask_cvtt_roundps_epu32 (m512i, mmask16, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512i = _mm512_maskz_cvtt_roundps_epu32 (mmask16, m512, 3); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_fixupimm_round_pd (m512d, m512d, m512i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_fixupimm_round_pd (m512d, mmask8, m512d, m512i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_fixupimm_round_pd (mmask8, m512d, m512d, m512i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_fixupimm_round_ps (m512, m512, m512i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_fixupimm_round_ps (m512, mmask16, m512, m512i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_fixupimm_round_ps (mmask16, m512, m512, m512i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_fixupimm_round_sd (m128d, m128d, m128i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_fixupimm_round_sd (m128d, mmask8, m128d, m128i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_fixupimm_round_sd (mmask8, m128d, m128d, m128i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_fixupimm_round_ss (m128, m128, m128i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_fixupimm_round_ss (m128, mmask8, m128, m128i, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_fixupimm_round_ss (mmask8, m128, m128, m128i, 4, 3); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvtt_roundss_u32 (m128, 3); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvtt_roundss_i32 (m128, 3); /* { dg-error "incorrect rounding operand" } */

  ui = _mm_cvtt_roundsd_u32 (m128d, 3); /* { dg-error "incorrect rounding operand" } */
  i = _mm_cvtt_roundsd_i32 (m128d, 3); /* { dg-error "incorrect rounding operand" } */

  m512d = _mm512_cvt_roundps_pd (m256, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_cvt_roundps_pd (m512d, mmask8, m256, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_cvt_roundps_pd (mmask8, m256, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_cvt_roundph_ps (m256i, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_cvt_roundph_ps (m512, mmask16, m256i, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_cvt_roundph_ps (mmask16, m256i, 3); /* { dg-error "incorrect rounding operand" } */

  m128d = _mm_cvt_roundss_sd (m128d, m128, 3); /* { dg-error "incorrect rounding operand" } */

  m128 = _mm_getexp_round_ss (m128, m128, 3); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_mask_getexp_round_ss (m128, mmask8, m128, m128, 3); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_maskz_getexp_round_ss (mmask8, m128, m128, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_getexp_round_sd (m128d, m128d, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_mask_getexp_round_sd (m128d, mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_maskz_getexp_round_sd (mmask8, m128d, m128d, 5); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_getexp_round_ps (m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_getexp_round_ps (m512, mmask16, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_getexp_round_ps (mmask16, m512, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_getexp_round_pd (m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_getexp_round_pd (m512d, mmask8, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_getexp_round_pd (mmask8, m512d, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_getmant_round_pd (m512d, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_getmant_round_pd (m512d, mmask8, m512d, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_getmant_round_pd (mmask8, m512d, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_getmant_round_ps (m512, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_getmant_round_ps (m512, mmask16, m512, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_getmant_round_ps (mmask16, m512, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_getmant_round_sd (m128d, m128d, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_getmant_round_ss (m128, m128, 0, 0, 3); /* { dg-error "incorrect rounding operand" } */

  m512 = _mm512_roundscale_round_ps (m512, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_mask_roundscale_round_ps (m512, mmask16, m512, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512 = _mm512_maskz_roundscale_round_ps (mmask16, m512, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_roundscale_round_pd (m512d, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_mask_roundscale_round_pd (m512d, mmask8, m512d, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m512d = _mm512_maskz_roundscale_round_pd (mmask8, m512d, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128 = _mm_roundscale_round_ss (m128, m128, 4, 3); /* { dg-error "incorrect rounding operand" } */
  m128d = _mm_roundscale_round_sd (m128d, m128d, 4, 3); /* { dg-error "incorrect rounding operand" } */

  mmask8 = _mm512_cmp_round_pd_mask (m512d, m512d, 4, 3); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm512_cmp_round_ps_mask (m512, m512, 4, 3); /* { dg-error "incorrect rounding operand" } */
  mmask8 = _mm512_mask_cmp_round_pd_mask (mmask8, m512d, m512d, 4, 3); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm512_mask_cmp_round_ps_mask (mmask16, m512, m512, 4, 3); /* { dg-error "incorrect rounding operand" } */
  mmask8 = _mm_cmp_round_sd_mask (m128d, m128d, 4, 3); /* { dg-error "incorrect rounding operand" } */
  mmask8 = _mm_mask_cmp_round_sd_mask (mmask8, m128d, m128d, 4, 3); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm_cmp_round_ss_mask (m128, m128, 4, 3); /* { dg-error "incorrect rounding operand" } */
  mmask16 = _mm_mask_cmp_round_ss_mask (mmask8, m128, m128, 4, 3); /* { dg-error "incorrect rounding operand" } */

  i = _mm_comi_round_ss (m128, m128, 4, 3); /* { dg-error "incorrect rounding operand" } */
  i = _mm_comi_round_sd (m128d, m128d, 4, 3); /* { dg-error "incorrect rounding operand" } */
}
