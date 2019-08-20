/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmovup\[sd]\t" } } */
/* { dg-final { scan-assembler "\tvextractf128\t" } } */

#include <immintrin.h>

void
foo (double *hi, double *lo, __m256d a)
{
  _mm256_storeu2_m128d (hi, lo, a);
}
