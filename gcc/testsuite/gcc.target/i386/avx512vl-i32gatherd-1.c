/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpgatherdd\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpgatherdd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256i x1, idx1;
volatile __m128i x2, idx2;
volatile __mmask8 m8;
int *base;

void extern
avx512vl_test (void)
{
  x1 = _mm256_mmask_i32gather_epi32 (x1, 0xFF, idx1, base, 8);
  x1 = _mm256_mmask_i32gather_epi32 (x1, m8, idx1, base, 8);
  x2 = _mm_mmask_i32gather_epi32 (x2, 0xFF, idx2, base, 8);
  x2 = _mm_mmask_i32gather_epi32 (x2, m8, idx2, base, 8);
}
