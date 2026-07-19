/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vcvtbf82bf4s\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82bf4s\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82bf4s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82bf4s\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82bf4s\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82bf4s\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128i;
volatile __m256i x256i;
volatile __m512i x512i;

void extern
avx10v2aux_vcvtbf82bf4s_test (void)
{
  x128i = _mm_cvts_bf8_bf4 (x128i);
  x128i = _mm256_cvts_bf8_bf4 (x256i);
  x256i = _mm512_cvts_bf8_bf4 (x512i);
}

void extern
avx10v2aux_vcvthf82bf4s_test (void)
{
  x128i = _mm_cvts_hf8_bf4 (x128i);
  x128i = _mm256_cvts_hf8_bf4 (x256i);
  x256i = _mm512_cvts_hf8_bf4 (x512i);
}
