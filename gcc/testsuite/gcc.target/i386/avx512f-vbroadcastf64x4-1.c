/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastf64x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]|vshuff64x2\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf64x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]|vshuff64x2\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf64x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}|vshuff64x2\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __m256d y;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_broadcast_f64x4 (y);
  x = _mm512_mask_broadcast_f64x4 (x, m, y);
  x = _mm512_maskz_broadcast_f64x4 (m, y);
}
