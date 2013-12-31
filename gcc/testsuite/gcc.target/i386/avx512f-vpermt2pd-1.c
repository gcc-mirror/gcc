/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpermt2pd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vpermt2pd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpermt2pd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __m512i y;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_permutex2var_pd (x, y, x);
  x = _mm512_mask_permutex2var_pd (x, m, y, x);
  x = _mm512_maskz_permutex2var_pd (m, x, y, x);
}
