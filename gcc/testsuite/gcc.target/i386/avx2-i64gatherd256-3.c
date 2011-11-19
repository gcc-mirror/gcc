/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpgatherqd\[ \\t\]+\[^\n\]*xmm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*xmm\[0-9\]" } } */

#include <immintrin.h>

__m128i x;
int *base;
__m256i idx;

void extern
avx2_test (void)
{
  x = _mm256_mask_i64gather_epi32 (x, base, idx, x, 1);
}
