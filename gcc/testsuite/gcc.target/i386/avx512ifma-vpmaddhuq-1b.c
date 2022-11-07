/* { dg-do compile } */
/* { dg-options "-mavx512ifma -mavx512vl -mavxifma -O2" } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52huq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52huq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpmadd52huq\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512i _x1, _y1, _z1;
volatile __m256i _x2, _y2, _z2;
volatile __m128i _x3, _y3, _z3;

void extern
avx512ifma_test (void)
{
  _x3 = _mm_madd52hi_epu64 (_x3, _y3, _z3);
  _x3 = _mm_mask_madd52hi_epu64 (_x3, 2, _y3, _z3);
  _x3 = _mm_maskz_madd52hi_epu64 (2, _x3, _y3, _z3);
  _x2 = _mm256_madd52hi_epu64 (_x2, _y2, _z2);
  _x2 = _mm256_mask_madd52hi_epu64 (_x2, 3, _y2, _z2);
  _x2 = _mm256_maskz_madd52hi_epu64 (3, _x2, _y2, _z2);
  _x1 = _mm512_madd52hi_epu64 (_x1, _y1, _z1);
  _x1 = _mm512_mask_madd52hi_epu64 (_x1, 3, _y1, _z1);
  _x1 = _mm512_maskz_madd52hi_epu64 (3, _x1, _y1, _z1);
}
