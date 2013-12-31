/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*\\)\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

long long *p;
volatile __m512i x1, x2;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_mask_mov_epi64 (x1, m, x2);
  x1 = _mm512_maskz_mov_epi64 (m, x2);

  x1 = _mm512_load_epi64 (p);
  x1 = _mm512_mask_load_epi64 (x1, m, p);
  x1 = _mm512_maskz_load_epi64 (m, p);

  _mm512_store_epi64 (p, x1);
  _mm512_mask_store_epi64 (p, m, x1);
}
