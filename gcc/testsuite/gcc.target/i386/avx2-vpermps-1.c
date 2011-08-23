/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vpermps\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

__m256 x;

void extern
avx2_test (void)
{
  x = _mm256_permutevar8x32_ps (x, x);
}
