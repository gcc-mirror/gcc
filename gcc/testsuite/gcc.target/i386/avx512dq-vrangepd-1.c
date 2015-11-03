/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512d z;
volatile __m256d y;
volatile __m128d x;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  z = _mm512_range_round_pd (z, z, 15, _MM_FROUND_NO_EXC);
  z = _mm512_range_pd (z, z, 15);
  y = _mm256_range_pd (y, y, 15);
  x = _mm_range_pd (x, x, 15);

  z = _mm512_mask_range_round_pd (z, m, z, z, 15, _MM_FROUND_NO_EXC);
  z = _mm512_mask_range_pd (z, m, z, z, 15);
  y = _mm256_mask_range_pd (y, m, y, y, 15);
  x = _mm_mask_range_pd (x, m, x, x, 15);

  z = _mm512_maskz_range_round_pd (m, z, z, 15, _MM_FROUND_NO_EXC);
  z = _mm512_maskz_range_pd (m, z, z, 15);
  y = _mm256_maskz_range_pd (m, y, y, 15);
  x = _mm_maskz_range_pd (m, x, x, 15);
}
