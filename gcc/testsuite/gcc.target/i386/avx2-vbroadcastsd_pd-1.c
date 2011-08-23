/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vbroadcastsd\[ \\t\]+\[^\n\]*" } } */

#include <immintrin.h>

__m128d x;
__m256d y;

void extern
avx2_test (void)
{
  y = _mm256_broadcastsd_pd (x);
}
