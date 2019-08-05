/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmovups\t" } } */
/* { dg-final { scan-assembler "\tvextractf128\t" } } */

#include <immintrin.h>

void
foo (float *hi, float *lo, __m256 a)
{
  _mm256_storeu2_m128 (hi, lo, a);
}
