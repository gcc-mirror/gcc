/* { dg-do compile } */
/* { dg-options "-mavx512f -mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfmulcph\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256h res1;
volatile __m128h res2;
volatile __m256h x1, x2, x3;
volatile __m128h x4, x5, x6;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res1 = _mm256_fmul_pch (x1, x2);
  res1 = _mm256_mask_fmul_pch (res1, m8, x1, x2);
  res1 = _mm256_maskz_fmul_pch (m8, x1, x2);

  res2 = _mm_fmul_pch (x4, x5);
  res2 = _mm_mask_fmul_pch (res2, m8, x4, x5);
  res2 = _mm_maskz_fmul_pch (m8, x4, x5);
  
  res1 = _mm256_mul_pch (x1, x2);
  res1 = _mm256_mask_mul_pch (res1, m8, x1, x2);
  res1 = _mm256_maskz_mul_pch (m8, x1, x2);

  res2 = _mm_mul_pch (x4, x5);
  res2 = _mm_mask_mul_pch (res2, m8, x4, x5);
  res2 = _mm_maskz_mul_pch (m8, x4, x5);
}
