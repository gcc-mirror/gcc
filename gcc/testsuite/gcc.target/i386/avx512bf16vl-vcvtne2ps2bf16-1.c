/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128bh res1;
volatile __m256bh res2;
volatile __m128 x1, x2;
volatile __m256 x3, x4;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx512bf16_test (void)
{
  res2 = _mm256_cvtne2ps_pbh (x3, x4);
  res2 = _mm256_mask_cvtne2ps_pbh (res2, m16, x3, x4);
  res2 = _mm256_maskz_cvtne2ps_pbh (m16, x3, x4);

  res1 = _mm_cvtne2ps_pbh (x1, x2);
  res1 = _mm_mask_cvtne2ps_pbh (res1, m8, x1, x2);
  res1 = _mm_maskz_cvtne2ps_pbh (m8, x1, x2);
}
