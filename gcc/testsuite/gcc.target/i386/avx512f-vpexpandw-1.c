/* { dg-do compile } */
/* { dg-options "-mavx512vbmi2 -mavx512bw -mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

int *p;
volatile __m512i x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_expand_epi16 (x, m, x);
  x = _mm512_maskz_expand_epi16 (m, x);

  x = _mm512_mask_expandloadu_epi16 (x, m, p);
  x = _mm512_maskz_expandloadu_epi16 (m, p);
}
