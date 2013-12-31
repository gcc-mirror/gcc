/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler "vprorvd\[ \\t\]+\[^\n\]*%zmm\[0-9\]" } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_rorv_epi32 (x, x);
  x = _mm512_mask_rorv_epi32 (x, m, x, x);
  x = _mm512_maskz_rorv_epi32 (m, x, x);
}
