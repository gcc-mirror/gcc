/* PR target/109174 */
/* { dg-do compile } */
/* { dg-options "-c -Wsign-conversion -Werror -mavx512bw -O2"  } */

#include <immintrin.h>

extern unsigned int bar();

void foo()
{ 
  __m512i a, w;
  __mmask32 u1;
  __mmask16 u2;
  __mmask8 u3;
  
  _mm512_slli_epi64(a, bar());
  _mm512_mask_slli_epi64(w, u3, a, bar());
  _mm512_maskz_slli_epi64(u3, a, bar());
  _mm512_slli_epi32(a, bar());
  _mm512_mask_slli_epi32(w, u2, a, bar());
  _mm512_maskz_slli_epi32(u2, a, bar());
  _mm512_slli_epi16(a, bar());
  _mm512_mask_slli_epi16(w, u1, a, bar());
  _mm512_maskz_slli_epi16(u1, a, bar());

  _mm512_srai_epi64(a, bar());
  _mm512_mask_srai_epi64(w, u3, a, bar());
  _mm512_maskz_srai_epi64(u3, a, bar());
  _mm512_srai_epi32(a, bar());
  _mm512_mask_srai_epi32(w, u2, a, bar());
  _mm512_maskz_srai_epi32(u2, a, bar());
  _mm512_srai_epi16(a, bar());
  _mm512_mask_srai_epi16(w, u1, a, bar());
  _mm512_maskz_srai_epi16(u1, a, bar());

  _mm512_srli_epi64(a, bar());
  _mm512_mask_srli_epi64(w, u3, a, bar());
  _mm512_maskz_srli_epi64(u3, a, bar());
  _mm512_srli_epi32(a, bar());
  _mm512_mask_srli_epi32(w, u2, a, bar());
  _mm512_maskz_srli_epi32(u2, a, bar());
  _mm512_srli_epi16(a, bar());
  _mm512_mask_srli_epi16(w, u1, a, bar());
}

