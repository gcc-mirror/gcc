/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpmovm2q\[ \\t\]+\[^\{\n\]*%k\[1-7\]\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovm2q\[ \\t\]+\[^\{\n\]*%k\[1-7\]\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovm2q\[ \\t\]+\[^\{\n\]*%k\[1-7\]\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x512;
volatile __m256i x256;
volatile __m128i x128;
volatile __mmask8 m8;

void extern
avx512dq_test (void)
{
  x128 = _mm_movm_epi64 (m8);
  x256 = _mm256_movm_epi64 (m8);
  x512 = _mm512_movm_epi64 (m8);
}
