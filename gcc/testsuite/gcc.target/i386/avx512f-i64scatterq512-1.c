/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpscatterqq\[ \\t\]+\[^\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]\[^\n\]*{%k\[1-7\]}" 2 } } */

#include <immintrin.h>

volatile __m512i src, idx;
volatile __mmask8 m8;
long long *addr;

void extern
avx512f_test (void)
{
  _mm512_i64scatter_epi64 (addr, idx, src, 8);
  _mm512_mask_i64scatter_epi64 (addr, m8, idx, src, 8);
}
