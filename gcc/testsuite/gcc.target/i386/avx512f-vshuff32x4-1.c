/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vshuff32x4\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vshuff32x4\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vshuff32x4\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

__m512 x;

void extern
avx512f_test (void)
{
  x = _mm512_shuffle_f32x4 (x, x, 56);
  x = _mm512_mask_shuffle_f32x4 (x, 4, x, x, 56);
  x = _mm512_maskz_shuffle_f32x4 (6, x, x, 56);
}
