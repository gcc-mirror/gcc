/* { dg-do compile } */
/* { dg-options "-mavx512bmm -O2" } */
/* { dg-final { scan-assembler-times "vbmacor16x16x16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbmacxor16x16x16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbitrevb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x,y,z;
volatile __mmask64 m;

__m512i extern
avx512bmm_test (void)
{
  x = _mm512_bmacor16x16x16 (x, y, z);

  x = _mm512_bmacxor16x16x16 (x, y, z);

  x = _mm512_bitrev_epi8 (x);

  x = _mm512_mask_bitrev_epi8 (m, x, y);

  x = _mm512_maskz_bitrev_epi8 (m, x);
}
