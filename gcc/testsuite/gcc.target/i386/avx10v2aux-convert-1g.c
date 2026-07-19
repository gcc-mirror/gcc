/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2" } */
/* { dg-final { scan-assembler-times "vcvtbf82bf6s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82bf6s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82bf6s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82hf6s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82hf6s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82hf6s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128i;
volatile __m256i x256i;
volatile __m512i x512i;

void extern
avx10v2aux_vcvtbf42hf8_test (void)
{
  x128i = _mm_cvts_bf8_bf6 (x128i);
  x256i = _mm256_cvts_bf8_bf6 (x256i);
  x512i = _mm512_cvts_bf8_bf6 (x512i);

  x128i = _mm_cvts_hf8_hf6 (x128i);
  x256i = _mm256_cvts_hf8_hf6 (x256i);
  x512i = _mm512_cvts_hf8_hf6 (x512i);
}
