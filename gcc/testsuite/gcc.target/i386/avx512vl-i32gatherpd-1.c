/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vgatherdpd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*ymm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vgatherdpd\[ \\t\]+\[^\{\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256d x1;
volatile __m128d x2;
volatile __m128i idx;
volatile __mmask8 m8;
double *base;

void extern
avx512vl_test (void)
{
  x1 = _mm256_mmask_i32gather_pd (x1, 0xFF, idx, base, 8);
  x1 = _mm256_mmask_i32gather_pd (x1, m8, idx, base, 8);
  x2 = _mm_mmask_i32gather_pd (x2, 0xFF, idx, base, 8);
  x2 = _mm_mmask_i32gather_pd (x2, m8, idx, base, 8);
}
