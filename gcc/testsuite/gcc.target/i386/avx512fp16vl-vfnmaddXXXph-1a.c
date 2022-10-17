/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vfnmadd...ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vfnmadd...ph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vfnmadd231ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd231ph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...ph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...ph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256h yy, y2, y3;
volatile __m128h xx, x2, x3;
volatile __mmask8 m;
volatile __mmask16 m16;

void extern
avx512vl_test (void)
{
  yy = _mm256_mask_fnmadd_ph (yy, m16, y2, y3);
  xx = _mm_mask_fnmadd_ph (xx, m, x2, x3);

  y3 = _mm256_mask3_fnmadd_ph (yy, y2, y3, m16);
  x3 = _mm_mask3_fnmadd_ph (xx, x2, x3, m);

  yy = _mm256_maskz_fnmadd_ph (m16, yy, y2, y3);
  xx = _mm_maskz_fnmadd_ph (m, xx, x2, x3);
}
