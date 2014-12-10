/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vscatterqps\[ \\t\]+\[^\{\n\]*ymm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256 src;
volatile __m512i idx;
volatile __mmask8 m8;
float *addr;

void extern
avx512f_test (void)
{
  _mm512_i64scatter_ps (addr, idx, src, 8);
  _mm512_mask_i64scatter_ps (addr, m8, idx, src, 8);
}
