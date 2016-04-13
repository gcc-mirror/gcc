/* PR target/70293 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mtune=westmere -mavx512vl -O2" } */

typedef short __v8hi __attribute__((__vector_size__(16)));
typedef int __v8hu __attribute__((__vector_size__(16)));
typedef long __m128i __attribute__((__vector_size__(16)));
__m128i _mm_madd_epi16___B, _mm_mullo_epi16___A,
    scaled_bilinear_scanline_sse2_8888_8_8888_OVER_xmm_b,
    scaled_bilinear_scanline_sse2_8888_8_8888_OVER___trans_tmp_16,
    scaled_bilinear_scanline_sse2_8888_8_8888_OVER___trans_tmp_13;
int _mm_srli_epi16___B, scaled_bilinear_scanline_sse2_8888_8_8888_OVER_m,
    scaled_bilinear_scanline_sse2_8888_8_8888_OVER_dst,
    scaled_bilinear_scanline_sse2_8888_8_8888_OVER_wt;
__m128i _mm_set_epi16();
void _mm_cvtsi128_si32();
void
scaled_bilinear_scanline_sse2_8888_8_8888_OVER(int p1) {
  __m128i __trans_tmp_12, __trans_tmp_6, __trans_tmp_5, xmm_x = _mm_set_epi16();
  int mask;
  __trans_tmp_5 = (__m128i){scaled_bilinear_scanline_sse2_8888_8_8888_OVER_wt};
  __trans_tmp_6 = (__m128i)(__v8hi){p1, p1, p1, p1, p1, p1, p1, p1};
  while (scaled_bilinear_scanline_sse2_8888_8_8888_OVER_dst) {
    scaled_bilinear_scanline_sse2_8888_8_8888_OVER_m = mask++;
    if (scaled_bilinear_scanline_sse2_8888_8_8888_OVER_m) {
      __trans_tmp_12 =
          (__m128i)((__v8hu)_mm_mullo_epi16___A * (__v8hu)__trans_tmp_6);
      scaled_bilinear_scanline_sse2_8888_8_8888_OVER_xmm_b = __trans_tmp_12;
      scaled_bilinear_scanline_sse2_8888_8_8888_OVER___trans_tmp_13 =
          (__m128i)__builtin_ia32_psrlwi128((__v8hi)xmm_x, _mm_srli_epi16___B);
      scaled_bilinear_scanline_sse2_8888_8_8888_OVER___trans_tmp_16 =
          (__m128i)__builtin_ia32_pmaddwd128((__v8hi)__trans_tmp_5,
                                             (__v8hi)_mm_madd_epi16___B);
      _mm_cvtsi128_si32();
    }
  }
}
