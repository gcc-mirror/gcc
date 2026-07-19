/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf42hf8\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128i;
volatile __m256i x256i;
volatile __m512i x512i;
volatile __mmask16 m16;
volatile __mmask32 m32;
volatile __mmask64 m64;

void extern
avx10v2aux_vcvtbf42hf8_test (void)
{
  x128i = _mm_cvtbf4_hf8 (x128i);
  x128i = _mm_mask_cvtbf4_hf8 (x128i, m16, x128i);
  x128i = _mm_maskz_cvtbf4_hf8 (m16, x128i);

  x256i = _mm256_cvtbf4_hf8 (x128i);
  x256i = _mm256_mask_cvtbf4_hf8 (x256i, m32, x128i);
  x256i = _mm256_maskz_cvtbf4_hf8 (m32, x128i);

  x512i = _mm512_cvtbf4_hf8 (x256i);
  x512i = _mm512_mask_cvtbf4_hf8 (x512i, m64, x256i);
  x512i = _mm512_maskz_cvtbf4_hf8 (m64, x256i);
}
