/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

float *p;
volatile __m512 x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_expand_ps (x, m, x);
  x = _mm512_maskz_expand_ps (m, x);

  x = _mm512_mask_expandloadu_ps (x, m, p);
  x = _mm512_maskz_expandloadu_ps (m, p);
}
