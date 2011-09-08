/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpmaskmovd\[ \\t\]+\[^\n\]" } } */

#include <immintrin.h>

__m128i x;
int *y;

void extern
avx2_test (void)
{
  x = _mm_maskload_epi32 (y, x);
}
