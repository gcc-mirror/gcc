/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpcmpuw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

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
  mmm = _mm512_cmp_epu16_mask (xq, xq, _MM_CMPINT_NE);
  mmm = _mm512_mask_cmp_epu16_mask (m, xq, xq, _MM_CMPINT_NLT);
  mm = _mm256_cmp_epu16_mask (x, x, _MM_CMPINT_GE);
  mm = _mm256_mask_cmp_epu16_mask (m, x, x, _MM_CMPINT_NLE);
  m = _mm_cmp_epu16_mask (xx, xx, _MM_CMPINT_GT);
  m = _mm_mask_cmp_epu16_mask (m, xx, xx, _MM_CMPINT_EQ);
}
