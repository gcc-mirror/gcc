/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*%ymm\[0-9\]" 6 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%ymm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ps\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%ymm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __m256 y;

void extern
avx512f_test (void)
{
  y = _mm512_cvtpd_ps (x);
  y = _mm512_mask_cvtpd_ps (y, 4, x);
  y = _mm512_maskz_cvtpd_ps (6, x);
  y = _mm512_cvt_roundpd_ps (x, _MM_FROUND_TO_NEAREST_INT);
  y = _mm512_mask_cvt_roundpd_ps (y, 4, x, _MM_FROUND_TO_NEG_INF);
  y = _mm512_maskz_cvt_roundpd_ps (6, x, _MM_FROUND_TO_ZERO);
}
