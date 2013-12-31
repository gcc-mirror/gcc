/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vshufpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vshufpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vshufpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

__m512d x;

void extern
avx512f_test (void)
{
  x = _mm512_shuffle_pd (x, x, 56);
  x = _mm512_mask_shuffle_pd (x, 2, x, x, 56);
  x = _mm512_maskz_shuffle_pd (2, x, x, 56);
}
