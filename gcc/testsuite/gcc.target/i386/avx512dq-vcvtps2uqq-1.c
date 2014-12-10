/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2uqq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x1;
volatile __m256i x2;
volatile __m128i x3;
volatile __m256 z1;
volatile __m128 z2;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  x1 = _mm512_cvtps_epu64 (z1);
  x1 = _mm512_mask_cvtps_epu64 (x1, m, z1);
  x1 = _mm512_maskz_cvtps_epu64 (m, z1);
  x2 = _mm256_cvtps_epu64 (z2);
  x2 = _mm256_mask_cvtps_epu64 (x2, m, z2);
  x2 = _mm256_maskz_cvtps_epu64 (m, z2);
  x3 = _mm_cvtps_epu64 (z2);
  x3 = _mm_mask_cvtps_epu64 (x3, m, z2);
  x3 = _mm_maskz_cvtps_epu64 (m, z2);
  x1 = _mm512_cvt_roundps_epu64 (z1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x1 = _mm512_mask_cvt_roundps_epu64 (x1, m, z1, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x1 = _mm512_maskz_cvt_roundps_epu64 (m, z1, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
