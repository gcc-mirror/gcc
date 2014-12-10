/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpsllw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
volatile __m512i x;
volatile __m256i x256;
volatile __m128i x128;
volatile __mmask32 m;
volatile __mmask16 m256;
volatile __mmask8 m128;
#define y 7

void extern
avx512bw_test (void)
{
  x = _mm512_slli_epi16 (x, y);
  x = _mm512_mask_slli_epi16 (x, m, x, y);
  x = _mm512_maskz_slli_epi16 (m, x, y);
  x256 = _mm256_mask_slli_epi16 (x256, m256, x256, y);
  x256 = _mm256_maskz_slli_epi16 (m256, x256, y);
  x128 = _mm_mask_slli_epi16 (x128, m128, x128, y);
  x128 = _mm_maskz_slli_epi16 (m128, x128, y);
}
