/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 x;
volatile __m128 xx;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  m = _mm256_cmp_ps_mask (x, x, _CMP_FALSE_OQ);
  m = _mm256_mask_cmp_ps_mask (m, x, x, _CMP_FALSE_OQ);
  m = _mm_cmp_ps_mask (xx, xx, _CMP_FALSE_OQ);
  m = _mm_mask_cmp_ps_mask (m, xx, xx, _CMP_FALSE_OQ);
}
