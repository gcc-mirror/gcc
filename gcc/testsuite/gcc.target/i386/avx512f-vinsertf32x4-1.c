/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\]*zmm" 3 } } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\]*\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vinsertf32x4\[^\n\]*\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512 x;
__m128 y;

void extern
avx512f_test (void)
{
  x = _mm512_insertf32x4 (x, y, 1);
  x = _mm512_maskz_insertf32x4 (6, x, y, 1);
  x = _mm512_mask_insertf32x4 (x, 2, x, y, 1);
}
