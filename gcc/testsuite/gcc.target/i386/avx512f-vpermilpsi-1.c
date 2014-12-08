/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\{\n\]*13\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\{\n\]*13\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\{\n\]*13\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_permute_ps (x, 13);
  x = _mm512_mask_permute_ps (x, m, x, 13);
  x = _mm512_maskz_permute_ps (m, x, 13);
}
