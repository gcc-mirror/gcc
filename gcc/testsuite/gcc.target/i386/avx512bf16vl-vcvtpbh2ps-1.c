/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\n\]*%ymm\[0-9\](?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$16, %ymm\[0-9]\+, %ymm\[0-9]\+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$16, %ymm\[0-9]\+, %ymm\[0-9]\+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$16, %xmm\[0-9]\+, %xmm\[0-9]\+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$16, %xmm\[0-9]\+, %xmm\[0-9]\+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128bh x1;
volatile __m128 res1;
volatile __m256 res2;
volatile __mmask8 m8;

void extern
avx512bf16_test (void)
{
  res2 = _mm256_cvtpbh_ps (x1);
  res2 = _mm256_mask_cvtpbh_ps (res2, m8, x1);
  res2 = _mm256_maskz_cvtpbh_ps (m8, x1);
  
  res1 = _mm_cvtpbh_ps (x1);
  res1 = _mm_mask_cvtpbh_ps (res1, m8, x1);
  res1 = _mm_maskz_cvtpbh_ps (m8, x1);
}
