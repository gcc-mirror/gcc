/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpgatherdq\[ \\t\]+\[^\n\]*ymm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*ymm\[0-9\]" } } */

#include <immintrin.h>

__m256i x;
long long int *base;
__m128i idx;

void extern
avx2_test (void)
{
  x = _mm256_mask_i32gather_epi64 (x, base, idx, x, 1);
}
