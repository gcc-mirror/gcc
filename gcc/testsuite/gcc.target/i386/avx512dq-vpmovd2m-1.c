/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpmovd2m\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovd2m\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovd2m\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x512;
volatile __m256i x256;
volatile __m128i x128;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx512dq_test (void)
{
  m8 = _mm_movepi32_mask (x128);
  m8 = _mm256_movepi32_mask (x256);
  m16 = _mm512_movepi32_mask (x512);
}
