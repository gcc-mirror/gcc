/* { dg-do compile } */
/* { dg-options "-O2 -msha512" } */
/* { dg-final { scan-assembler "vsha512msg1\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsha512msg2\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]" } } */
/* { dg-final { scan-assembler "vsha512rnds2\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]" } } */

#include <immintrin.h>

volatile __m128i x;
volatile __m256i y;

void extern
sha512_test (void)
{
  y = _mm256_sha512msg1_epi64(y, x);
  y = _mm256_sha512msg2_epi64(y, y);
  y = _mm256_sha512rnds2_epi64(y, y, x);
}
