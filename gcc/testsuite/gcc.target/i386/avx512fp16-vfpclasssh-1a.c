/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vfpclasssh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfpclasssh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h x128;
volatile __mmask8 m8;

void extern
avx512dq_test (void)
{
  m8 = _mm_fpclass_sh_mask (x128, 13);
  m8 = _mm_mask_fpclass_sh_mask (m8, x128, 13);
}
