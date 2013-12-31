/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*\\)\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

float *p;
volatile __m512 x1, x2;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_mask_mov_ps (x1, m, x2);
  x1 = _mm512_maskz_mov_ps (m, x2);

  x1 = _mm512_load_ps (p);
  x1 = _mm512_mask_load_ps (x1, m, p);
  x1 = _mm512_maskz_load_ps (m, p);

  _mm512_store_ps (p, x1);
  _mm512_mask_store_ps (p, m, x1);
}
