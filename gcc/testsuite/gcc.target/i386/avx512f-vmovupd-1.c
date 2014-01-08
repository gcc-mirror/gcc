/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovupd\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovupd\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovupd\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovupd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*\\)\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovupd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*\\)\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

double *p;
volatile __m512d x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_loadu_pd (p);
  x = _mm512_mask_loadu_pd (x, m, p);
  x = _mm512_maskz_loadu_pd (m, p);

  _mm512_storeu_pd (p, x);
  _mm512_mask_storeu_pd (p, m, x);
}
