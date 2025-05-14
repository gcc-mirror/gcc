/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x256;
volatile __m128i x128;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  m = _mm_cmpgt_epu64_mask (x128, x128);
  m = _mm256_cmpgt_epu64_mask (x256, x256);
  m = _mm_mask_cmpgt_epu64_mask (5, x128, x128);
  m = _mm256_mask_cmpgt_epu64_mask (5, x256, x256);
}
