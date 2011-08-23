/* { dg-do compile } */
/* { dg-options "-mavx2 -O3" } */
/* { dg-final { scan-assembler "vpsllw\[ \\t\]+\[^\n\]*\\$\[0-9\]" } } */

#include <immintrin.h>

volatile __m256i x;

void extern
avx2_test (void)
{
  x = _mm256_slli_epi16 (x, 13);
}
