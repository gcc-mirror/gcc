/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O0" } */

#include <immintrin.h>

volatile __m128i x128i;
volatile __m256i x256i;
volatile __m512i x512i;
volatile __mmask16 m16;
volatile __mmask32 m32;
volatile __mmask64 m64;

void extern
avx10v2aux_vunpack_imm_range_test (void)
{
  x128i = _mm_unpack_epi8 (x128i, 1); /* { dg-error "the last argument must not use reserved value immediate" } */
  x128i = _mm_mask_unpack_epi8 (x128i, m16, x128i, 14); /* { dg-error "the last argument must not use reserved value immediate" } */
  x128i = _mm_maskz_unpack_epi8 (m16, x128i, 21); /* { dg-error "the last argument must not use reserved value immediate" } */

  x256i = _mm256_unpack_epi8 (x256i, 22); /* { dg-error "the last argument must not use reserved value immediate" } */
  x256i = _mm256_mask_unpack_epi8 (x256i, m32, x256i, 25); /* { dg-error "the last argument must not use reserved value immediate" } */
  x256i = _mm256_maskz_unpack_epi8 (m32, x256i, 29); /* { dg-error "the last argument must not use reserved value immediate" } */

  x512i = _mm512_unpack_epi8 (x512i, 30); /* { dg-error "the last argument must not use reserved value immediate" } */
  x512i = _mm512_mask_unpack_epi8 (x512i, m64, x512i, 31); /* { dg-error "the last argument must not use reserved value immediate" } */
  x512i = _mm512_maskz_unpack_epi8 (m64, x512i, 50); /* { dg-error "the last argument must not use reserved value immediate" } */
}
