/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vgatherqps\[ \\t\]+\[^\n\]*xmm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*xmm\[0-9\]" } } */

#include <immintrin.h>

__m128 x;
float *base;
__m256i idx;

void extern
avx2_test (void)
{
  x = _mm256_mask_i64gather_ps (x, base, idx, x, 1);
}
