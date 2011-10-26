/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpgatherqq\[ \\t\]+\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;
long long int *base;
__m256i idx;

void extern
avx2_test (void)
{
  x = _mm256_i64gather_epi64 (base, idx, 1);
}
