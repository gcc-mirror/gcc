/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

#define IMM 123

volatile __m128h x1, x2;
volatile __mmask8 m;

void extern
avx512fp16_test (void)
{
  x1 = _mm_roundscale_sh (x1, x2, IMM);
  x1 = _mm_mask_roundscale_sh(x1, m, x1, x2, IMM);
  x1 = _mm_maskz_roundscale_sh(m, x1, x2, IMM);
  x1 = _mm_roundscale_round_sh (x1, x2, IMM, 4);
  x1 = _mm_mask_roundscale_round_sh(x1, m, x1, x2, IMM, 8);
  x1 = _mm_maskz_roundscale_round_sh(m, x1, x2, IMM, 8);
}
