/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vbroadcasti128\[ \\t\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;
__m128i y;

void extern
avx2_test (void)
{
  x = _mm256_broadcastsi128_si256 (y);
}
