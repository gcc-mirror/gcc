/* { dg-do compile } */
/* { dg-options "-mavxifma -O2" } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52huq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52luq\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52huq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "\{vex\} vpmadd52luq\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+" 1 } } */

#include <immintrin.h>

volatile __m256i x,y,z;
volatile __m128i x_,y_,z_;

void extern
avxifma_test (void)
{
  x = _mm256_madd52hi_epu64 (x, y, z);
  x = _mm256_madd52lo_epu64 (x, y, z);
  x_ = _mm_madd52hi_epu64 (x_, y_, z_);
  x_ = _mm_madd52lo_epu64 (x_, y_, z_);
}
