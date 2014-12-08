/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vbroadcastf64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\]|vshuff64x2\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\[^\{\]|vshuff64x2\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}|vshuff64x2\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\]|vshuff64x2\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\[^\{\]|vshuff64x2\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}|vshuff64x2\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __m256d y;
volatile __m128d z;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  x = _mm512_broadcast_f64x2 (z);
  x = _mm512_mask_broadcast_f64x2 (x, m, z);
  x = _mm512_maskz_broadcast_f64x2 (m, z);
  y = _mm256_broadcast_f64x2 (z);
  y = _mm256_mask_broadcast_f64x2 (y, m, z);
  y = _mm256_maskz_broadcast_f64x2 (m, z);
}
