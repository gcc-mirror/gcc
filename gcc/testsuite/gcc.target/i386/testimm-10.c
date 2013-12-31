/* { dg-do compile } */
/* { dg-options "-O0 -mavx512f" } */

#include <x86intrin.h>

__m512i m512i;
__m512d m512d;
__m512  m512;
__m256i m256i;
__m256d m256d;
__m256  m256;
__m128i m128i;
__m128d m128d;
__m128  m128;
__mmask8 mmask8;
__mmask16 mmask16;

void
test8bit (void)
{
  m512i = _mm512_permutex_epi64 (m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_permutex_epi64 (m512i, mmask8, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_permutex_epi64 (mmask8, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512i = _mm512_ternarylogic_epi64 (m512i, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_ternarylogic_epi64 (m512i, mmask8, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_ternarylogic_epi64 (mmask8, m512i, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_ternarylogic_epi32 (m512i, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_ternarylogic_epi32 (m512i, mmask16, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_ternarylogic_epi32 (mmask16, m512i, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512i = _mm512_shuffle_epi32 (m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_shuffle_epi32 (m512i, mmask16, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_shuffle_epi32 (mmask16, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512i = _mm512_shuffle_i64x2 (m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_shuffle_i64x2 (m512i, mmask8, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_shuffle_i64x2 (mmask8, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512i = _mm512_shuffle_i32x4 (m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_shuffle_i32x4 (m512i, mmask16, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_shuffle_i32x4 (mmask16, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512d = _mm512_shuffle_f64x2 (m512d, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_mask_shuffle_f64x2 (m512d, mmask8, m512d, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_maskz_shuffle_f64x2 (mmask8, m512d, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512  = _mm512_shuffle_f32x4 (m512, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512  = _mm512_mask_shuffle_f32x4 (m512, mmask16, m512, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512  = _mm512_maskz_shuffle_f32x4 (mmask16, m512, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512d = _mm512_permutex_pd (m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_mask_permutex_pd (m512d, mmask8, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_maskz_permutex_pd (mmask8, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512d = _mm512_permute_pd (m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_mask_permute_pd (m512d, mmask8, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_maskz_permute_pd (mmask8, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512  = _mm512_permute_ps (m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512  = _mm512_mask_permute_ps (m512, mmask16, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512  = _mm512_maskz_permute_ps (mmask16, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512d = _mm512_shuffle_pd (m512d, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_mask_shuffle_pd (m512d, mmask8, m512d, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512d = _mm512_maskz_shuffle_pd (mmask8, m512d, m512d, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512  = _mm512_shuffle_ps (m512, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512  = _mm512_mask_shuffle_ps (m512, mmask16, m512, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512  = _mm512_maskz_shuffle_ps (mmask16, m512, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512d = _mm512_fixupimm_pd (m512d, m512d, m512i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512d = _mm512_mask_fixupimm_pd (m512d, mmask8, m512d, m512i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512d = _mm512_maskz_fixupimm_pd (mmask8, m512d, m512d, m512i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */

  m512  = _mm512_fixupimm_ps (m512, m512, m512i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512  = _mm512_mask_fixupimm_ps (m512, mmask16, m512, m512i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512  = _mm512_maskz_fixupimm_ps (mmask16, m512, m512, m512i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */

  m128d = _mm_fixupimm_sd (m128d, m128d, m128i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m128d = _mm_mask_fixupimm_sd (m128d, mmask8, m128d, m128i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m128d = _mm_maskz_fixupimm_sd (mmask8, m128d, m128d, m128i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */

  m128  = _mm_fixupimm_ss (m128, m128, m128i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m128  = _mm_mask_fixupimm_ss (m128, mmask8, m128, m128i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m128  = _mm_maskz_fixupimm_ss (mmask8, m128, m128, m128i, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */

  m512i = _mm512_rol_epi32 (m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_rol_epi32 (m512i, mmask16, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_rol_epi32 (mmask16, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512i = _mm512_ror_epi32 (m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_ror_epi32 (m512i, mmask16, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_ror_epi32 (mmask16, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512i = _mm512_rol_epi64 (m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_rol_epi64 (m512i, mmask8, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_rol_epi64 (mmask8, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512i = _mm512_ror_epi64 (m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_ror_epi64 (m512i, mmask8, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_ror_epi64 (mmask8, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m256i = _mm512_cvtps_ph (m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m256i = _mm512_mask_cvtps_ph (m256i, mmask16, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m256i = _mm512_maskz_cvtps_ph (mmask16, m512, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  m512d = _mm512_roundscale_pd (m512d, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512d = _mm512_mask_roundscale_pd (m512d, mmask8, m512d, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512d = _mm512_maskz_roundscale_pd (mmask8, m512d, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */

  m512  = _mm512_roundscale_ps (m512, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512  = _mm512_mask_roundscale_ps (m512, mmask16, m512, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m512  = _mm512_maskz_roundscale_ps (mmask16, m512, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */

  m128d = _mm_roundscale_sd (m128d, m128d, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */
  m128  = _mm_roundscale_ss (m128, m128, 256); /* { dg-error "the immediate argument must be an 8-bit immediate" } */

  m512i = _mm512_alignr_epi32 (m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_alignr_epi32 (m512i, mmask16, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_alignr_epi32 (mmask16, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_alignr_epi64 (m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_mask_alignr_epi64 (m512i, mmask8, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */
  m512i = _mm512_maskz_alignr_epi64 (mmask8, m512i, m512i, 256); /* { dg-error "the last argument must be an 8-bit immediate" } */

  mmask8 = _mm512_cmp_epi64_mask (m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_cmp_epi32_mask (m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_cmp_epu64_mask (m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_cmp_epu32_mask (m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_cmp_pd_mask (m512d, m512d, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */
  mmask8 = _mm512_cmp_ps_mask (m512, m512, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */
  mmask8 = _mm512_mask_cmp_epi64_mask (2, m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_mask_cmp_epi32_mask (2, m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_mask_cmp_epu64_mask (2, m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_mask_cmp_epu32_mask (2, m512i, m512i, 256); /* { dg-error "the last argument must be a 3-bit immediate" } */
  mmask8 = _mm512_mask_cmp_pd_mask (2, m512d, m512d, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */
  mmask8 = _mm512_mask_cmp_ps_mask (2, m512, m512, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */
  mmask8 = _mm_cmp_sd_mask (m128d, m128d, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */
  mmask8 = _mm_cmp_ss_mask (m128, m128, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */
  mmask8 = _mm_mask_cmp_sd_mask (1, m128d, m128d, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */
  mmask8 = _mm_mask_cmp_ss_mask (1, m128, m128, 256); /* { dg-error "the immediate argument must be a 5-bit immediate" } */

}

test1bit (void) {
  m256d = _mm512_extractf64x4_pd (m512d, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m256d = _mm512_mask_extractf64x4_pd (m256d, mmask8, m512d, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m256d = _mm512_maskz_extractf64x4_pd (mmask8, m512d, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */

  m256i = _mm512_extracti64x4_epi64 (m512i, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m256i = _mm512_mask_extracti64x4_epi64 (m256i, mmask8, m512i, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m256i = _mm512_maskz_extracti64x4_epi64 (mmask8, m512i, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */

  m512d = _mm512_insertf64x4 (m512d, m256d, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m512d = _mm512_mask_insertf64x4 (m512d, mmask8, m512d, m256d, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m512d = _mm512_maskz_insertf64x4 (mmask8, m512d, m256d, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */

  m512i = _mm512_inserti64x4 (m512i, m256i, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m512i = _mm512_mask_inserti64x4 (m512i, mmask8, m512i, m256i, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
  m512i = _mm512_maskz_inserti64x4 (mmask8, m512i, m256i, 256); /* { dg-error "the last argument must be a 1-bit immediate" } */
}

test2bit (void) {
  m128  = _mm512_extractf32x4_ps(m512, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m128  = _mm512_mask_extractf32x4_ps(m128, mmask8, m512, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m128  = _mm512_maskz_extractf32x4_ps(mmask8, m512, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */

  m128i = _mm512_extracti32x4_epi32 (m512i, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m128i = _mm512_mask_extracti32x4_epi32 (m128i, mmask8, m512i, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m128i = _mm512_maskz_extracti32x4_epi32 (mmask8, m512i, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */

  m512  = _mm512_insertf32x4 (m512, m128, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m512  = _mm512_mask_insertf32x4 (m512, mmask16, m512, m128, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m512  = _mm512_maskz_insertf32x4 (mmask16, m512, m128, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */

  m512i = _mm512_inserti32x4 (m512i, m128i, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m512i = _mm512_mask_inserti32x4 (m512i, mmask16, m512i, m128i, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
  m512i = _mm512_maskz_inserti32x4 (mmask16, m512i, m128i, 256); /* { dg-error "the last argument must be a 2-bit immediate" } */
}

test4bit (void) {
  m512d = _mm512_getmant_pd (m512d, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */
  m512d = _mm512_mask_getmant_pd (m512d, mmask8, m512d, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */
  m512d = _mm512_maskz_getmant_pd (mmask8, m512d, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */

  m512  = _mm512_getmant_ps (m512, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */
  m512  = _mm512_mask_getmant_ps (m512, mmask16, m512, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */
  m512  = _mm512_maskz_getmant_ps (mmask16, m512, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */

  m128d = _mm_getmant_sd (m128d, m128d, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */
  m128  = _mm_getmant_ss (m128, m128, 1, 64); /* { dg-error "the immediate argument must be a 4-bit immediate" } */
}
