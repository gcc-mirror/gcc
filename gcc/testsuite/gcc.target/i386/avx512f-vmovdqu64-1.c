/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*\\)\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu64\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */

#include <immintrin.h>

long long *p;
volatile __m512i x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_loadu_epi64 (x, m, p);
  x = _mm512_maskz_loadu_epi64 (m, p);

  _mm512_mask_storeu_epi64 (p, m, x);
}
