/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[\\n\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%zmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512 s;
volatile __m512i res;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvtps_epu32 (s);
  res = _mm512_mask_cvtps_epu32 (res, m, s);
  res = _mm512_maskz_cvtps_epu32 (m, s);
  res = _mm512_cvt_roundps_epu32 (s, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  res = _mm512_mask_cvt_roundps_epu32 (res, m, s, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  res = _mm512_maskz_cvt_roundps_epu32 (m, s, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
