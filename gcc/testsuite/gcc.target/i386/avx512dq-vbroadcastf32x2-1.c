/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastf32x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf32x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m256 y;
volatile __m128 z;
volatile __mmask16 mx;
volatile __mmask8 my;

void extern
avx512dq_test (void)
{
  x = _mm512_broadcast_f32x2 (z);
  x = _mm512_mask_broadcast_f32x2 (x, mx, z);
  x = _mm512_maskz_broadcast_f32x2 (mx, z);
  y = _mm256_broadcast_f32x2 (z);
  y = _mm256_mask_broadcast_f32x2 (y, my, z);
  y = _mm256_maskz_broadcast_f32x2 (my, z);
}
