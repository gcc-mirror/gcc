/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovapd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

double *p;
volatile __m512d x1, x2;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_mask_mov_pd (x1, m, x2);
  x1 = _mm512_maskz_mov_pd (m, x2);

  x1 = _mm512_load_pd (p);
  x1 = _mm512_mask_load_pd (x1, m, p);
  x1 = _mm512_maskz_load_pd (m, p);

  _mm512_store_pd (p, x1);
  _mm512_mask_store_pd (p, m, x1);
}
