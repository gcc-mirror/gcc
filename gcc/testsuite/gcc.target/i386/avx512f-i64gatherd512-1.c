/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpgatherqd\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*ymm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m512i idx;
volatile __mmask8 m8;
int *base;

void extern
avx512f_test (void)
{
  x = _mm512_i64gather_epi32 (idx, base, 8);
  x = _mm512_mask_i64gather_epi32 (x, m8, idx, base, 8);
}
