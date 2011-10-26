/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpgatherdd\[ \\t\]+\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;
int *base;
__m256i idx;

void extern
avx2_test (void)
{
  x = _mm256_i32gather_epi32 (base, idx, 1);
}
