/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpermilps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m512i c;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_permutevar_ps (x, c);
  x = _mm512_mask_permutevar_ps (x, m, x, c);
  x = _mm512_maskz_permutevar_ps (m, x, c);
}
