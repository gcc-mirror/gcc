/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vgatherdpd\[ \\t\]+\[^\n\]*ymm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*ymm\[0-9\]" } } */

#include <immintrin.h>

__m256d x;
double *base;
__m128i idx;

void extern
avx2_test (void)
{
  x = _mm256_i32gather_pd (base, idx, 1);
}
