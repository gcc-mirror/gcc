/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpcmpw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i xq;
volatile __m256i x;
volatile __m128i xx;
volatile __mmask8 m;
volatile __mmask16 mm;
volatile __mmask32 mmm;

void extern
avx512bw_test (void)
{
  mmm = _mm512_cmp_epi16_mask (xq, xq, _MM_CMPINT_GT);
  mmm = _mm512_mask_cmp_epi16_mask (m, xq, xq, _MM_CMPINT_EQ);
  mm = _mm256_cmp_epi16_mask (x, x, _MM_CMPINT_EQ);
  mm = _mm256_mask_cmp_epi16_mask (m, x, x, _MM_CMPINT_LT);
  m = _mm_cmp_epi16_mask (xx, xx, _MM_CMPINT_LE);
  m = _mm_mask_cmp_epi16_mask (m, xx, xx, _MM_CMPINT_UNUSED);
}
