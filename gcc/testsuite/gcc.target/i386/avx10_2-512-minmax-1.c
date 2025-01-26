/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  2 } } */


#include <immintrin.h>

volatile __m512bh x1;
volatile __m512h x2;
volatile __m512 x3;
volatile __m512d x4;
volatile __mmask32 m32;
volatile __mmask16 m16;
volatile __mmask8 m8;

void extern
avx10_2_512_test (void)
{
  x1 = _mm512_minmax_pbh (x1, x1, 100);
  x1 = _mm512_mask_minmax_pbh (x1, m32, x1, x1, 100);
  x1 = _mm512_maskz_minmax_pbh (m32, x1, x1, 100);
  x2 = _mm512_minmax_ph (x2, x2, 1);
  x2 = _mm512_mask_minmax_ph (x2, m32, x2, x2, 1);
  x2 = _mm512_maskz_minmax_ph (m32, x2, x2, 1);
  x2 = _mm512_minmax_round_ph (x2, x2, 1, 4);
  x2 = _mm512_mask_minmax_round_ph (x2, m32, x2, x2, 1, 4);
  x2 = _mm512_maskz_minmax_round_ph (m32, x2, x2, 1, 4);
  x3 = _mm512_minmax_ps (x3, x3, 1);
  x3 = _mm512_mask_minmax_ps (x3, m16, x3, x3, 1);
  x3 = _mm512_maskz_minmax_ps (m16, x3, x3, 1);
  x3 = _mm512_minmax_round_ps (x3, x3, 1, 4);
  x3 = _mm512_mask_minmax_round_ps (x3, m16, x3, x3, 1, 4);
  x3 = _mm512_maskz_minmax_round_ps (m16, x3, x3, 1, 4);
  x4 = _mm512_minmax_pd (x4, x4, 100);
  x4 = _mm512_mask_minmax_pd (x4, m8, x4, x4, 100);
  x4 = _mm512_maskz_minmax_pd (m8, x4, x4, 100);
  x4 = _mm512_minmax_round_pd (x4, x4, 100, 4);
  x4 = _mm512_mask_minmax_round_pd (x4, m8, x4, x4, 100, 4);
  x4 = _mm512_maskz_minmax_round_pd (m8, x4, x4, 100, 4);
}
