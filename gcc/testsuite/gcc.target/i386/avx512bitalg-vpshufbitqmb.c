/* { dg-do compile } */
/* { dg-options "-mavx512bitalg -mavx512vl -mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "vpshufbitqmb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshufbitqmb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshufbitqmb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshufbitqmb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshufbitqmb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshufbitqmb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128;
volatile __m256i x256;
volatile __m512i x512;

volatile __mmask16 m16;
volatile __mmask32 m32;
volatile __mmask64 m64;

void extern
avx512vl_test (void)
{
  m16 = _mm_bitshuffle_epi64_mask (x128, x128);
  m32 = _mm256_bitshuffle_epi64_mask (x256, x256);
  m64 = _mm512_bitshuffle_epi64_mask (x512, x512);
  m16 = _mm_mask_bitshuffle_epi64_mask (m16, x128, x128);
  m32 = _mm256_mask_bitshuffle_epi64_mask (m32, x256, x256);
  m64 = _mm512_mask_bitshuffle_epi64_mask (m64, x512, x512);
}
