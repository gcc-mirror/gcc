/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512vbmi2 -mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "vpshldvw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshldvq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x,y,z;
volatile __m128i x_,y_,z_;
volatile __mmask32 m;

void extern
avx512f_test (void)
{
  x = _mm256_shldv_epi16 (x, y, z);
  x = _mm256_mask_shldv_epi16 (x, m, y, z);
  x = _mm256_maskz_shldv_epi16 (m, x, y, z);

  x = _mm256_shldv_epi32 (x, y, z);
  x = _mm256_mask_shldv_epi32 (x, m, y, z);
  x = _mm256_maskz_shldv_epi32 (m, x, y, z);

  x = _mm256_shldv_epi64 (x, y, z);
  x = _mm256_mask_shldv_epi64 (x, m, y, z);
  x = _mm256_maskz_shldv_epi64 (m, x, y, z);

  x_ = _mm_shldv_epi16 (x_, y_, z_);
  x_ = _mm_mask_shldv_epi16 (x_, m, y_, z_);
  x_ = _mm_maskz_shldv_epi16 (m, x_, y_, z_);

  x_ = _mm_shldv_epi32 (x_, y_, z_);
  x_ = _mm_mask_shldv_epi32 (x_, m, y_, z_);
  x_ = _mm_maskz_shldv_epi32 (m, x_, y_, z_);

  x_ = _mm_shldv_epi64 (x_, y_, z_);
  x_ = _mm_mask_shldv_epi64 (x_, m, y_, z_);
  x_ = _mm_maskz_shldv_epi64 (m, x_, y_, z_);
}
