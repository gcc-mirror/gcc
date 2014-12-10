/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vscatterqpd\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscatterqpd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256d src1;
volatile __m128d src2;
volatile __m256i idx1;
volatile __m128i idx2;
volatile __mmask8 m8;
double *addr;

void extern
avx512vl_test (void)
{
  _mm256_i64scatter_pd (addr, idx1, src1, 8);
  _mm256_mask_i64scatter_pd (addr, m8, idx1, src1, 8);

  _mm_i64scatter_pd (addr, idx2, src2, 8);
  _mm_mask_i64scatter_pd (addr, m8, idx2, src2, 8);
}
