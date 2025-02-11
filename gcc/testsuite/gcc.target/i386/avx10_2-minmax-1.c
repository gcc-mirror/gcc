/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-256" } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxbf16\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxph\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxps\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\{\n\]*\[^\}\]%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxpd\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsh\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsh\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsh\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsh\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsh\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsh\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxss\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxss\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxss\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxss\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxss\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxss\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsd\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsd\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsd\[ \\t\]+\[^\{\n\]*\[^\}\]%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsd\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsd\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vminmaxsd\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m256bh y1_;
volatile __m256h y2;
volatile __m256 y3;
volatile __m256d y4;
volatile __m128bh x1;
volatile __m128h x2;
volatile __m128 x3;
volatile __m128d x4;
volatile __mmask16 m16;
volatile __mmask8 m8;

void extern
avx10_2_test (void)
{
  x1 = _mm_minmax_pbh (x1, x1, 100);
  x1 = _mm_mask_minmax_pbh (x1, m8, x1, x1, 100);
  x1 = _mm_maskz_minmax_pbh (m8, x1, x1, 100);
  y1_ = _mm256_minmax_pbh (y1_, y1_, 100);
  y1_ = _mm256_mask_minmax_pbh (y1_, m16, y1_, y1_, 100);
  y1_ = _mm256_maskz_minmax_pbh (m16, y1_, y1_, 100);
  x2 = _mm_minmax_ph (x2, x2, 100);
  x2 = _mm_mask_minmax_ph (x2, m8, x2, x2, 100);
  x2 = _mm_maskz_minmax_ph (m8, x2, x2, 100);
  y2 = _mm256_minmax_ph (y2, y2, 100);
  y2 = _mm256_mask_minmax_ph (y2, m16, y2, y2, 100);
  y2 = _mm256_maskz_minmax_ph (m16, y2, y2, 100);
  y2 = _mm256_minmax_round_ph (y2, y2, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  y2 = _mm256_mask_minmax_round_ph (y2, m16, y2, y2, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  y2 = _mm256_maskz_minmax_round_ph (m16, y2, y2, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x3 = _mm_minmax_ps (x3, x3, 100);
  x3 = _mm_mask_minmax_ps (x3, m8, x3, x3, 100);
  x3 = _mm_maskz_minmax_ps (m8, x3, x3, 100);
  y3 = _mm256_minmax_ps (y3, y3, 100);
  y3 = _mm256_mask_minmax_ps (y3, m8, y3, y3, 100);
  y3 = _mm256_maskz_minmax_ps (m8, y3, y3, 100);
  y3 = _mm256_minmax_round_ps (y3, y3, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  y3 = _mm256_mask_minmax_round_ps (y3, m8, y3, y3, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  y3 = _mm256_maskz_minmax_round_ps (m8, y3, y3, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x4 = _mm_minmax_pd (x4, x4, 100);
  x4 = _mm_mask_minmax_pd (x4, m8, x4, x4, 100);
  x4 = _mm_maskz_minmax_pd (m8, x4, x4, 100);
  y4 = _mm256_minmax_pd (y4, y4, 100);
  y4 = _mm256_mask_minmax_pd (y4, m8, y4, y4, 100);
  y4 = _mm256_maskz_minmax_pd (m8, y4, y4, 100);
  y4 = _mm256_minmax_round_pd (y4, y4, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  y4 = _mm256_mask_minmax_round_pd (y4, m8, y4, y4, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  y4 = _mm256_maskz_minmax_round_pd (m8, y4, y4, 100, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x2 = _mm_minmax_sh (x2, x2, 1);
  x2 = _mm_mask_minmax_sh (x2, m8, x2, x2, 1);
  x2 = _mm_maskz_minmax_sh (m8, x2, x2, 1);
  x2 = _mm_minmax_round_sh (x2, x2, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x2 = _mm_mask_minmax_round_sh (x2, m8, x2, x2, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x2 = _mm_maskz_minmax_round_sh (m8, x2, x2, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x3 = _mm_minmax_ss (x3, x3, 1);
  x3 = _mm_mask_minmax_ss (x3, m8, x3, x3, 1);
  x3 = _mm_maskz_minmax_ss (m8, x3, x3, 1);
  x3 = _mm_minmax_round_ss (x3, x3, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x3 = _mm_mask_minmax_round_ss (x3, m8, x3, x3, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x3 = _mm_maskz_minmax_round_ss (m8, x3, x3, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x4 = _mm_minmax_sd (x4, x4, 1);
  x4 = _mm_mask_minmax_sd (x4, m8, x4, x4, 1);
  x4 = _mm_maskz_minmax_sd (m8, x4, x4, 1);
  x4 = _mm_minmax_round_sd (x4, x4, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x4 = _mm_mask_minmax_round_sd (x4, m8, x4, x4, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x4 = _mm_maskz_minmax_round_sd (m8, x4, x4, 1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
