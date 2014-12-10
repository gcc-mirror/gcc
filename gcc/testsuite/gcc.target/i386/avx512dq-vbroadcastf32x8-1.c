/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastf32x8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\]|vshuff32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\[^\{\]|vshuff32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}|vshuff32x4\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m256 y;
volatile __mmask16 m;

void extern
avx512dq_test (void)
{
  x = _mm512_broadcast_f32x8 (y);
  x = _mm512_mask_broadcast_f32x8 (x, m, y);
  x = _mm512_maskz_broadcast_f32x8 (m, y);
}
