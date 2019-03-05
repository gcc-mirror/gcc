/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512cd -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastmb2q\[ \\t\]+\[^\{\n\]*%k\[0-7\]\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastmb2q\[ \\t\]+\[^\{\n\]*%k\[0-7\]\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128;
volatile __m256i x256;
volatile __mmask8 m8;

void extern
avx512vl_test (void)
{
  x128 = _mm_broadcastmb_epi64 (m8);
  x256 = _mm256_broadcastmb_epi64 (m8);
}
