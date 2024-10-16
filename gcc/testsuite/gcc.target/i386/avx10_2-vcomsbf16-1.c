/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vcomsbf16\[ \\t\]+\[^{}\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6 } } */
/* { dg-final { scan-assembler-times "jp" 2 } } */
#include <immintrin.h>

volatile __m128bh x1, x2;
volatile int res;

void extern
avx10_2_vcom_test (void)
{
  res = _mm_comeq_sbh (x1, x2);
  res = _mm_comlt_sbh (x1, x2);
  res = _mm_comle_sbh (x1, x2);
  res = _mm_comgt_sbh (x1, x2);
  res = _mm_comge_sbh (x1, x2);
  res = _mm_comneq_sbh (x1, x2);
}
