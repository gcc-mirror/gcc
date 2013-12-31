/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[\\n\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\[\\n\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512 s;
volatile __m512i res;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  res = _mm512_cvttps_epi32 (s);
  res = _mm512_mask_cvttps_epi32 (res, m, s);
  res = _mm512_maskz_cvttps_epi32 (m, s);
  res = _mm512_cvtt_roundps_epi32 (s, _MM_FROUND_NO_EXC);
  res = _mm512_mask_cvtt_roundps_epi32 (res, m, s, _MM_FROUND_NO_EXC);
  res = _mm512_maskz_cvtt_roundps_epi32 (m, s, _MM_FROUND_NO_EXC);
}
