/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreducepd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

#define IMM 123

volatile __m256d x1;
volatile __m128d x2;
volatile __mmask8 m;

void extern
avx156p_test (void)
{
  x1 = _mm256_reduce_pd (x1, IMM);
  x2 = _mm_reduce_pd (x2, IMM);

  x1 = _mm256_mask_reduce_pd (x1, m, x1, IMM);
  x2 = _mm_mask_reduce_pd (x2, m, x2, IMM);

  x1 = _mm256_maskz_reduce_pd (m, x1, IMM);
  x2 = _mm_maskz_reduce_pd (m, x2, IMM);
}
