/* { dg-do compile } */
/* { dg-options "-mavx512vbmi2 -mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "vpshldw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\\n\\r]*%zmm\[0-9\]+\[^\\n\\r\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x,y;
volatile __mmask32 m32;
volatile __mmask16 m16;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  x = _mm512_shldi_epi16 (x, y, 3);
  x = _mm512_maskz_shldi_epi16 (m32, x, y, 3);
  x = _mm512_mask_shldi_epi16 (x, m32, y, x, 3);

  x = _mm512_shldi_epi32 (x, y, 3);
  x = _mm512_maskz_shldi_epi32 (m16, x, y, 3);
  x = _mm512_mask_shldi_epi32 (x, m16, y, x, 3);

  x = _mm512_shldi_epi64 (x, y, 3);
  x = _mm512_maskz_shldi_epi64 (m8, x, y, 3);
  x = _mm512_mask_shldi_epi64 (x, m8, y, x, 3);
}
