/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512vbmi2 -O2" } */
/* { dg-final { scan-assembler-times "vpcompressw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcompressw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcompressw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcompressw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcompressw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcompressw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

int *p;
volatile __m256i x1;
volatile __m128i x2;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx512vl_test (void)
{
  x1 = _mm256_mask_compress_epi16 (x1, m16, x1);
  x2 = _mm_mask_compress_epi16 (x2, m8, x2);

  x1 = _mm256_maskz_compress_epi16 (m16, x1);
  x2 = _mm_maskz_compress_epi16 (m8, x2);

  _mm256_mask_compressstoreu_epi16 (p, m16, x1);
  _mm_mask_compressstoreu_epi16 (p, m8, x2);
}
