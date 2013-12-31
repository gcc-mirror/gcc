/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastf32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]|vshuff32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]|vshuff32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}|vshuff32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m128 y;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_broadcast_f32x4 (y);
  x = _mm512_mask_broadcast_f32x4 (x, m, y);
  x = _mm512_maskz_broadcast_f32x4 (m, y);
}
