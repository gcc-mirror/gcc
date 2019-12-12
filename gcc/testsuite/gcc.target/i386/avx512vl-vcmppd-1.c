/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmppd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d x;
volatile __m128d xx;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  m = _mm256_cmp_pd_mask (x, x, _CMP_FALSE_OQ);
  m = _mm256_mask_cmp_pd_mask (m, x, x, _CMP_FALSE_OQ);
  m = _mm_cmp_pd_mask (xx, xx, _CMP_FALSE_OQ);
  m = _mm_mask_cmp_pd_mask (m, xx, xx, _CMP_FALSE_OQ);
}
