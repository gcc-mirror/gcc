/* { dg-do compile } */
/* { dg-options "-mavx2 -O3" } */
/* { dg-final { scan-assembler "vpsrlw\[ \\t\]+\[^\n\]*\\$\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;

void extern
avx2_test (void)
{
  x = _mm256_srli_epi16 (x, 13);
}
