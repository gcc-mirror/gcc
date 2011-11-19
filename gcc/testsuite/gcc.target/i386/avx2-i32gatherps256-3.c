/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vgatherdps\[ \\t\]+\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]\[^\n\]*ymm\[0-9\]" } } */

#include <immintrin.h>

__m256 x;
float *base;
__m256i idx;

void extern
avx2_test (void)
{
  x = _mm256_mask_i32gather_ps (x, base, idx, x, 1);
}
