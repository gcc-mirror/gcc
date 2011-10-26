/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vgatherqpd\[ \\t\]+\[^\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]" } } */

#include <immintrin.h>

__m128d x;
double *base;
__m128i idx;

void extern
avx2_test (void)
{
  x = _mm_mask_i64gather_pd (x, base, idx, x, 1);
}
