/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpgatherqq\[ \\t\]+\[^\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]\[^\n\]*xmm\[0-9\]" } } */

#include <immintrin.h>

__m128i x;
long long int *base;
__m128i idx;

void extern
avx2_test (void)
{
  x = _mm_i64gather_epi64 (base, idx, 1);
}
