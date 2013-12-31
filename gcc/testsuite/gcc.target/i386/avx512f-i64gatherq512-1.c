/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpgatherqq\[ \\t\]+\[^\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]{%k\[1-7\]}" 2 } } */

#include <immintrin.h>

volatile __m512i x, idx;
volatile __mmask8 m8;
long long *base;

void extern
avx512f_test (void)
{
  x = _mm512_i64gather_epi64 (idx, base, 8);
  x = _mm512_mask_i64gather_epi64 (x, m8, idx, base, 8);
}
