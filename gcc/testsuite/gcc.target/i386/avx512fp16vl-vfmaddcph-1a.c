/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "(?:vblendmps\[ \\t\]+%ymm\[0-9\]+|vmovaps\[ \\t\]+)\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "(?:vblendmps\[ \\t\]+%xmm\[0-9\]+|vmovaps\[ \\t\]+)\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256h res1;
volatile __m128h res2;
volatile __m256h x1, x2, x3;
volatile __m128h x4, x5, x6;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res1 = _mm256_fmadd_pch (x1, x2, x3);
  res1 = _mm256_mask_fmadd_pch (res1, m8, x1, x2);
  res1 = _mm256_mask3_fmadd_pch (res1, x1, x2, m8);
  res1 = _mm256_maskz_fmadd_pch (m8, x1, x2, x3);

  res2 = _mm_fmadd_pch (x4, x5, x6);
  res2 = _mm_mask_fmadd_pch (res2, m8, x4, x5);
  res2 = _mm_mask3_fmadd_pch (res2, x4, x5, m8);
  res2 = _mm_maskz_fmadd_pch (m8, x4, x5, x6);
}
