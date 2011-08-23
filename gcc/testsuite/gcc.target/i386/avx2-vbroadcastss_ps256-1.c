/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vbroadcastss\[ \\t\]+\[^\n\]*%xmm\[0-9\]" } } */

#include <immintrin.h>

__m128 x;
__m256 y;

void extern
avx2_test (void)
{
  y = _mm256_broadcastss_ps (x);
}
