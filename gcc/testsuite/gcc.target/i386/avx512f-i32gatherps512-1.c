/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -mtune=knl" } */
/* { dg-final { scan-assembler-times "vgatherdps\[ \\t\]+\[^\{\n\]*zmm\[0-9\]\[^\n\]*zmm\[0-9\]{%k\[1-7\]}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __m512i idx;
volatile __mmask16 m16;
float *base;

void extern
avx512f_test (void)
{
  x = _mm512_i32gather_ps (idx, base, 8);
  x = _mm512_mask_i32gather_ps (x, m16, idx, base, 8);
}
