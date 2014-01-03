/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vgatherqps\[ \\t\]+\[^\n\]*zmm\[0-9\]\[^\n\]*ymm\[0-9\]{%k\[1-7\]}" 2 } } */

#include <immintrin.h>

volatile __m256 x;
volatile __m512i idx;
volatile __mmask8 m8;
float *base;

void extern
avx512f_test (void)
{
  x = _mm512_i64gather_ps (idx, base, 8);
  x = _mm512_mask_i64gather_ps (x, m8, idx, base, 8);
}
