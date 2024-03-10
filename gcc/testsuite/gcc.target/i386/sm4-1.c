/* { dg-do compile } */
/* { dg-options "-O2 -msm4" } */
/* { dg-final { scan-assembler "vsm4key4\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsm4key4\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsm4rnds4\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsm4rnds4\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m128i a, b, c;
volatile __m256i d, e, f;

void extern
sm4_test (void)
{
  a = _mm_sm4key4_epi32 (b, c);
  d = _mm256_sm4key4_epi32 (e, f);
  a = _mm_sm4rnds4_epi32 (b, c);
  d = _mm256_sm4rnds4_epi32 (e, f);
}
