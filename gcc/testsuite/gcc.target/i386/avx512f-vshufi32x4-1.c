/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vshufi32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vshufi32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vshufi32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

__m512i x;

void extern
avx512f_test (void)
{
  x = _mm512_shuffle_i32x4 (x, x, 56);
  x = _mm512_mask_shuffle_i32x4 (x, 8, x, x, 56);
  x = _mm512_maskz_shuffle_i32x4 (8, x, x, 56);
}
