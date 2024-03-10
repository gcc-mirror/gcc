/* { dg-do compile } */
/* { dg-options "-mvaes -mavx512f -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vaesdec\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesdeclast\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesenc\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesenclast\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vaesdec\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesdeclast\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesenc\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesenclast\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vaesdec\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesdeclast\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesenc\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaesenclast\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m512i x,y;
volatile __m256i x256, y256;
volatile __m128i x128, y128;

void extern
avx512f_test (void)
{
  x = _mm512_aesdec_epi128 (x, y);
  x = _mm512_aesdeclast_epi128 (x, y);
  x = _mm512_aesenc_epi128 (x, y);
  x = _mm512_aesenclast_epi128 (x, y);

  x256 = _mm256_aesdec_epi128 (x256, y256);
  x256 = _mm256_aesdeclast_epi128 (x256, y256);
  x256 = _mm256_aesenc_epi128 (x256, y256);
  x256 = _mm256_aesenclast_epi128 (x256, y256);

  x128 = _mm_aesdec_si128 (x128, y128);
  x128 = _mm_aesdeclast_si128 (x128, y128);
  x128 = _mm_aesenc_si128 (x128, y128);
  x128 = _mm_aesenclast_si128 (x128, y128);
}
