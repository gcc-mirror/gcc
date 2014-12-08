/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpaddusb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpaddusb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpaddusb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpaddusb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpaddusb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpaddusb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpaddusb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x512;
volatile __m256i x256;
volatile __m128i x128;
volatile __mmask64 m512;
volatile __mmask32 m256;
volatile __mmask16 m128;

void extern
avx512bw_test (void)
{
  x512 = _mm512_adds_epu8 (x512, x512);
  x512 = _mm512_mask_adds_epu8 (x512, m512, x512, x512);
  x512 = _mm512_maskz_adds_epu8 (m512, x512, x512);
  x256 = _mm256_mask_adds_epu8 (x256, m256, x256, x256);
  x256 = _mm256_maskz_adds_epu8 (m256, x256, x256);
  x128 = _mm_mask_adds_epu8 (x128, m128, x128, x128);
  x128 = _mm_maskz_adds_epu8 (m128, x128, x128);
}
