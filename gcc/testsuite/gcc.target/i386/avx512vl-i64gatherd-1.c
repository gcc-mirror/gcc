/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpgatherqd\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*xmm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpgatherqd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256i idx1;
volatile __m128i idx2, x;
volatile __mmask8 m8;
int *base;

void extern
avx512vl_test (void)
{
  x = _mm256_mmask_i64gather_epi32 (x, 0xFF, idx1, base, 8);
  x = _mm256_mmask_i64gather_epi32 (x, m8, idx1, base, 8);
  x = _mm_mmask_i64gather_epi32 (x, 0xFF, idx2, base, 8);
  x = _mm_mmask_i64gather_epi32 (x, m8, idx2, base, 8);
}
