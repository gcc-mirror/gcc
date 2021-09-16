/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vmovsh\[ \\t\]+%xmm\[0-9\]+\[^\n\r\]*%\[er\]ax+\[^\n\r]*\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsh\[ \\t\]+\[^\n\r\]*%\[er\]ax+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsh\[ \\t\]+\[^\n\r\]*%\[er\]ax+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsh\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsh\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^z\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovsh\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

extern _Float16 const* p;
volatile __m128h x1, x2, res;
volatile __mmask8 m8;

void
avx512f_test (void)
{
  x2 = _mm_mask_load_sh (x1, m8, p);
  x2 = _mm_maskz_load_sh (m8, p);
  _mm_mask_store_sh (p, m8, x1);

  res = _mm_move_sh (x1, x2);
  res = _mm_mask_move_sh (res, m8, x1, x2);
  res = _mm_maskz_move_sh (m8, x1, x2);
}
