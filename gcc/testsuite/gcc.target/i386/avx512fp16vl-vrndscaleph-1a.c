/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscaleph\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

#define IMM 123

volatile __m256h x2;
volatile __m128h x3;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx512fp16_test (void)
{
  x2 = _mm256_roundscale_ph (x2, IMM);
  x3 = _mm_roundscale_ph (x3, IMM);

  x2 = _mm256_mask_roundscale_ph (x2, m16, x2, IMM);
  x3 = _mm_mask_roundscale_ph (x3, m8, x3, IMM);

  x2 = _mm256_maskz_roundscale_ph (m8, x2, IMM);
  x3 = _mm_maskz_roundscale_ph (m16, x3, IMM);
}
