/* { dg-do compile } */
/* { dg-options "-O2 -msm3" } */
/* { dg-final { scan-assembler "vsm3msg1\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsm3msg2\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsm3rnds2\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]" } } */

#include <immintrin.h>

volatile __m128i x, y, z;

void extern
sm3_test (void)
{
  x = _mm_sm3msg1_epi32 (x, y, z);
  x = _mm_sm3msg2_epi32 (x, y, z);
  x = _mm_sm3rnds2_epi32 (x, y, z, 1);
}
