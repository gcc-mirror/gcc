/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovdqa32\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa32\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa32\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa32\[ \\t\]+\[^\n\]*\\)\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa32\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*\\)\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

int *p;
volatile __m512i x1, x2;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_mask_mov_epi32 (x1, m, x2);
  x1 = _mm512_maskz_mov_epi32 (m, x2);

  x1 = _mm512_load_si512 (p);
  x1 = _mm512_load_epi32 (p);
  x1 = _mm512_mask_load_epi32 (x1, m, p);
  x1 = _mm512_maskz_load_epi32 (m, p);

  _mm512_store_si512 (p, x1);
  _mm512_store_epi32 (p, x1);
  _mm512_mask_store_epi32 (p, m, x1);
}
