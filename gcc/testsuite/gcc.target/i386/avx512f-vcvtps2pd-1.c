/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2pd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 s;
volatile __m512d res;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtps_pd (s);
  res = _mm512_mask_cvtps_pd (res, m, s);
  res = _mm512_maskz_cvtps_pd (m, s);
  res = _mm512_cvt_roundps_pd (s, _MM_FROUND_NO_EXC);
  res = _mm512_mask_cvt_roundps_pd (res, m, s, _MM_FROUND_NO_EXC);
  res = _mm512_maskz_cvt_roundps_pd (m, s, _MM_FROUND_NO_EXC);
}
