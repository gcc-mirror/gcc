/* PR target/109173 */
/* { dg-do compile } */
/* { dg-options "-c -Wsign-conversion -Werror -mavx512bw -mavx512vl -O2"  } */

#include <immintrin.h>

extern unsigned int bar();

void foo()
{
  __m128i a1, w1;
  __m256i a2, w2;
  __mmask8 u;

  _mm256_mask_srli_epi32(w2, u, a2, bar());
  _mm256_maskz_srli_epi32(u, a2, bar());
  _mm_mask_srli_epi32(w1, u, a1, bar());
  _mm_maskz_srli_epi32(u, a1, bar());

  _mm256_mask_srli_epi64(w2, u, a2, bar());
  _mm256_maskz_srli_epi64(u, a2, bar());
  _mm_mask_srli_epi64(w1, u, a1, bar());
  _mm_maskz_srli_epi64(u, a1, bar());

  _mm256_mask_srai_epi32(w2, u, a2, bar());
  _mm256_maskz_srai_epi32(u, a2, bar());
  _mm_mask_srai_epi32(w1, u, a1, bar());
  _mm_maskz_srai_epi32(u, a1, bar());

  _mm256_srai_epi64(a2, bar());
  _mm256_mask_srai_epi64(w2, u, a2, bar());
  _mm256_maskz_srai_epi64(u, a2, bar());
  _mm_srai_epi64(a1, bar());
  _mm_mask_srai_epi64(w1, u, a1, bar());
  _mm_maskz_srai_epi64(u, a1, bar());

  _mm256_mask_slli_epi32(w2, u, a2, bar());
  _mm256_maskz_slli_epi32(u, a2, bar());
  _mm_mask_slli_epi32(w1, u, a1, bar());
  _mm_maskz_slli_epi32(u, a1, bar());

  _mm256_mask_slli_epi64(w2, u, a2, bar());
  _mm256_maskz_slli_epi64(u, a2, bar());
  _mm_mask_slli_epi64(w1, u, a1, bar());
  _mm_maskz_slli_epi64(u, a1, bar());

  _mm256_mask_srai_epi16(w2, u, a2, bar());
  _mm256_maskz_srai_epi16(u, a2, bar());
  _mm_mask_srai_epi16(w1, u, a1, bar());
  _mm_maskz_srai_epi16(u, a1, bar());

  _mm256_mask_slli_epi16(w2, u, a2, bar());
  _mm256_maskz_slli_epi16(u, a2, bar());
  _mm_mask_slli_epi16(w1, u, a1, bar());
  _mm_maskz_slli_epi16(u, a1, bar());
}

