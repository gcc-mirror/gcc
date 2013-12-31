/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vscatterdps\[ \\t\]+\[^\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}" 2 } } */

#include <immintrin.h>

volatile __m512 src;
volatile __m512i idx;
volatile __mmask16 m16;
float *addr;

void extern
avx512f_test (void)
{
  _mm512_i32scatter_ps (addr, idx, src, 8);
  _mm512_mask_i32scatter_ps (addr, m16, idx, src, 8);
}
