/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vpmovm2q\[ \\t\]+\[^\{\n\]*%k\[0-7\]\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovm2q\[ \\t\]+\[^\{\n\]*%k\[0-7\]\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x256;
volatile __m128i x128;
volatile __mmask8 m;

void extern
avx10_1_test (void)
{
  x128 = _mm_movm_epi64 (m);
  x256 = _mm256_movm_epi64 (m);
}
