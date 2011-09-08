/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpmaskmovq\[ \\t\]+\[^\n\]" } } */

#include <immintrin.h>

__m128i x;
long long int *y;

void extern
avx2_test (void)
{
  x = _mm_maskload_epi64 (y, x);
}
