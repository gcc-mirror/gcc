/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -masm=att" } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+\\(%\[a-z0-9,]*\\), %xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+\\(%\[a-z0-9,]*\\), %xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+%xmm\[0-9\]+, %xmm\[0-9\]+, %xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+%xmm\[0-9\]+, %xmm\[0-9\]+, %xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+%xmm\[0-9\]+, \\(%\[a-z0-9,]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x1, x2, x3;
volatile __mmask8 m;
float *volatile p;

void extern
avx512f_test (void)
{
  x1 = _mm_mask_load_ss (x1, m, p);
  x1 = _mm_maskz_load_ss (m, p);
  x1 = _mm_mask_move_ss (x1, m, x2, x3);
  x1 = _mm_maskz_move_ss (m, x2, x3);
  _mm_mask_store_ss (p, m, x1);
}
