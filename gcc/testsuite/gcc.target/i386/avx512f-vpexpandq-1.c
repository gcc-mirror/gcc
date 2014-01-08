/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */

#include <immintrin.h>

long long *p;
volatile __m512i x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_expand_epi64 (x, m, x);
  x = _mm512_maskz_expand_epi64 (m, x);

  x = _mm512_mask_expandloadu_epi64 (x, m, p);
  x = _mm512_maskz_expandloadu_epi64 (m, p);
}
