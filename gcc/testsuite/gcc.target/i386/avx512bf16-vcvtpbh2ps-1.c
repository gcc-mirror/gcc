/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -O2" } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\n\]*%zmm\[0-9\](?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$16, %zmm\[0-9]\+, %zmm\[0-9]\+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$16, %zmm\[0-9]\+, %zmm\[0-9]\+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovsxwd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256bh x1;
volatile __m512 res;
volatile __mmask16 m16;

void extern
avx512bf16_test (void)
{
  res = _mm512_cvtpbh_ps (x1);
  res = _mm512_mask_cvtpbh_ps (res, m16, x1);
  res = _mm512_maskz_cvtpbh_ps (m16, x1);
}
