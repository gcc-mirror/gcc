/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovddup\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]|vunpcklpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vmovddup\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]|vunpcklpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovddup\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}|vunpcklpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512d x1, x2;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
    x1 = _mm512_movedup_pd (x2);
    x1 = _mm512_mask_movedup_pd (x1, m8, x2);
    x1 = _mm512_maskz_movedup_pd (m8, x2);
}
