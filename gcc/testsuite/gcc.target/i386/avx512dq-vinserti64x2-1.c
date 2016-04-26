/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vinserti64x2\[^\n\]*zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m512i z;
volatile __m256i x;
volatile __m128i y;

void extern
avx512dq_test (void)
{
  x = _mm256_inserti64x2 (x, y, 1);
  x = _mm256_mask_inserti64x2 (x, 2, x, y, 1);
  x = _mm256_maskz_inserti64x2 (2, x, y, 1);
  z = _mm512_inserti64x2 (z, y, 0);
  z = _mm512_mask_inserti64x2 (z, 2, z, y, 0);
  z = _mm512_maskz_inserti64x2 (2, z, y, 0);
}
