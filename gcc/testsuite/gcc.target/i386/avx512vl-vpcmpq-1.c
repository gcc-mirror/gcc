/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpcmpq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m128i xx;
volatile __mmask8 m;

void extern
avx512bw_test (void)
{
  m = _mm256_cmp_epi64_mask (x, x, _MM_CMPINT_UNUSED);
  m = _mm256_mask_cmp_epi64_mask (m, x, x, _MM_CMPINT_NE);
  m = _mm_cmp_epi64_mask (xx, xx, _MM_CMPINT_NLT);
  m = _mm_mask_cmp_epi64_mask (m, xx, xx, _MM_CMPINT_GE);
}
