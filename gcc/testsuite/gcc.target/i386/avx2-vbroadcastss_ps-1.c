/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler "vbroadcastss\[ \\t\]+\[^\n\]*%xmm\[0-9\]" } } */

#include <immintrin.h>

__m128 x;

void extern
avx2_test (void)
{
  x = _mm_broadcastss_ps (x);
}
