/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vptestmw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vptestmw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vptestmw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vptestmw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vptestmw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vptestmw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128;
volatile __m256i x256;
volatile __m512i x512;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;

void extern
avx512bw_test (void)
{
  m8 = _mm_test_epi16_mask (x128, x128);
  m16 = _mm256_test_epi16_mask (x256, x256);
  m32 = _mm512_test_epi16_mask (x512, x512);
  m8 = _mm_mask_test_epi16_mask (3, x128, x128);
  m16 = _mm256_mask_test_epi16_mask (3, x256, x256);
  m32 = _mm512_mask_test_epi16_mask (3, x512, x512);
}
