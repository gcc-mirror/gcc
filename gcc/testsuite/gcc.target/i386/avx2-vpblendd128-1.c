/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpblendd\[ \\t\]+\[^\n\]*" } } */

#include <immintrin.h>

__m128i x;

void extern
avx2_test (void)
{
  x = _mm_blend_epi32 (x, x, 13);
}
