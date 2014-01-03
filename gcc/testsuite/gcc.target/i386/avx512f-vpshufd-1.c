/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpshufd\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vpshufd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vpshufd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512i x;

void extern
avx512f_test (void)
{
  x = _mm512_shuffle_epi32 (x, _MM_PERM_AADB);
  x = _mm512_mask_shuffle_epi32 (x, 2, x, _MM_PERM_AADB);
  x = _mm512_maskz_shuffle_epi32 (2, x, _MM_PERM_AADB);
}
