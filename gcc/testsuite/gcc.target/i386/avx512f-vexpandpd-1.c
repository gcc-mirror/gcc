/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 4 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */

#include <immintrin.h>

double *p;
volatile __m512d x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_expand_pd (x, m, x);
  x = _mm512_maskz_expand_pd (m, x);

  x = _mm512_mask_expandloadu_pd (x, m, p);
  x = _mm512_maskz_expandloadu_pd (m, p);
}
