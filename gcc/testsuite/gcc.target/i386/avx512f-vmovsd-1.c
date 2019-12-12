/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -masm=att" } */
/* { dg-final { scan-assembler-times "vmovsd\[ \\t\]+\\(%\[a-z0-9,]*\\), %xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsd\[ \\t\]+\\(%\[a-z0-9,]*\\), %xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsd\[ \\t\]+%xmm\[0-9\]+, %xmm\[0-9\]+, %xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsd\[ \\t\]+%xmm\[0-9\]+, %xmm\[0-9\]+, %xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsd\[ \\t\]+%xmm\[0-9\]+, \\(%\[a-z0-9,]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x1, x2, x3;
volatile __mmask8 m;
double *volatile p;

void extern
avx512f_test (void)
{
  x1 = _mm_mask_load_sd (x1, m, p);
  x1 = _mm_maskz_load_sd (m, p);
  x1 = _mm_mask_move_sd (x1, m, x2, x3);
  x1 = _mm_maskz_move_sd (m, x2, x3);
  _mm_mask_store_sd (p, m, x1);
}
