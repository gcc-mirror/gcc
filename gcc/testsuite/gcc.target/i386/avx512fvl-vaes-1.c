/* { dg-do compile } */
/* { dg-options "-mvaes -mavx512f -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vaesdec\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vaesdec\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vaesdec\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m512i x,y;
volatile __m256i x256, y256;
volatile __m128i x128, y128;

void extern
avx512f_test (void)
{
  x = _mm512_aesdec_epi128 (x, y);

  x256 = _mm256_aesdec_epi128 (x256, y256);

  x128 = _mm_aesdec_epi128 (x128, y128);

}
