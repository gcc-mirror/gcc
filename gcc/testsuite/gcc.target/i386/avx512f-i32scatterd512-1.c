/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpscatterdd\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m512i src, idx;
volatile __mmask16 m16;
int *addr;

void extern
avx512f_test (void)
{
  _mm512_i32scatter_epi32 (addr, idx, src, 8);
  _mm512_mask_i32scatter_epi32 (addr, m16, idx, src, 8);
}
