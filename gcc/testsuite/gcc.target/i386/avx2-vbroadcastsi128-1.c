/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times "vbroadcasti128\[ \\t\]+\[^\n\]*%ymm\[0-9\]+" 2 } } */

#include <immintrin.h>

volatile __m256i x,xx;
__m128i y,yy;

void extern
avx2_test (void)
{
  x = _mm256_broadcastsi128_si256 (y);
  xx = _mm_broadcastsi128_si256 (yy);
}
