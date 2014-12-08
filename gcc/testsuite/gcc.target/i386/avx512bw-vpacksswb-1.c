/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpacksswb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpacksswb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpacksswb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpacksswb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpacksswb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpacksswb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpacksswb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */


#include <immintrin.h>

volatile __m512i x;
volatile __m256i y;
volatile __m128i z;
volatile __mmask64 mx;
volatile __mmask32 my;
volatile __mmask16 mz;

void extern
avx512bw_test (void)
{
  x = _mm512_packs_epi16 (x, x);
  x = _mm512_mask_packs_epi16 (x, mx, x, x);
  x = _mm512_maskz_packs_epi16 (mx, x, x);
  y = _mm256_mask_packs_epi16 (y, my, y, y);
  y = _mm256_maskz_packs_epi16 (my, y, y);
  z = _mm_mask_packs_epi16 (z, mz, z, z);
  z = _mm_maskz_packs_epi16 (mz, z, z);
}
