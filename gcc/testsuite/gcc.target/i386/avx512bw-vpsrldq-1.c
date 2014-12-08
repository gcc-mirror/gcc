/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "vpsrldq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

extern volatile __m512i x;

void extern
avx512bw_test (void)
{
  x = _mm512_bsrli_epi128 (x, 13);
}
