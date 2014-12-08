/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpcmpb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i xq;
volatile __m256i x;
volatile __m128i xx;
volatile __mmask16 m;
volatile __mmask32 mm;
volatile __mmask64 mmm;

void extern
avx512bw_test (void)
{
  mmm = _mm512_cmp_epi8_mask (xq, xq, _MM_CMPINT_GE);
  mmm = _mm512_mask_cmp_epi8_mask (m, xq, xq, _MM_CMPINT_NLE);
  mm = _mm256_cmp_epi8_mask (x, x, _MM_CMPINT_GT);
  mm = _mm256_mask_cmp_epi8_mask (m, x, x, _MM_CMPINT_EQ);
  m = _mm_cmp_epi8_mask (xx, xx, _MM_CMPINT_LT);
  m = _mm_mask_cmp_epi8_mask (m, xx, xx, _MM_CMPINT_LE);
}
