/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512vbmi2 -O2" } */
/* { dg-final { scan-assembler-times "vpshrdw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdw\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdd\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshrdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x,y;
volatile __m128i z1,z2;
volatile __mmask32 m;

void extern
avx512f_test (void)
{
  x = _mm256_shrdi_epi16 (x, y, 3);
  x = _mm256_maskz_shrdi_epi16 (m, x, y, 3);
  x = _mm256_mask_shrdi_epi16 (x, m, y, x, 3);

  x = _mm256_shrdi_epi32 (x, y, 3);
  x = _mm256_maskz_shrdi_epi32 (m, x, y, 3);
  x = _mm256_mask_shrdi_epi32 (x, m, y, x, 3);

  x = _mm256_shrdi_epi64 (x, y, 3);
  x = _mm256_maskz_shrdi_epi64 (m, x, y, 3);
  x = _mm256_mask_shrdi_epi64 (x, m, y, x, 3);

  z1 = _mm_shrdi_epi16 (z1, z2, 3);
  z1 = _mm_maskz_shrdi_epi16 (m, z1, z2, 3);
  z1 = _mm_mask_shrdi_epi16 (z1, m, z2, z1, 3);

  z1 = _mm_shrdi_epi32 (z1, z2, 3);
  z1 = _mm_maskz_shrdi_epi32 (m, z1, z2, 3);
  z1 = _mm_mask_shrdi_epi32 (z1, m, z2, z1, 3);

  z1 = _mm_shrdi_epi64 (z1, z2, 3);
  z1 = _mm_maskz_shrdi_epi64 (m, z1, z2, 3);
  z1 = _mm_mask_shrdi_epi64 (z1, m, z2, z1, 3);
}
