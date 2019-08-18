/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmovups\t" } } */
/* { dg-final { scan-assembler "\tvinsertf128\t" } } */

#include <immintrin.h>

__m256
foo (float const *hi, float const *lo)
{
  return _mm256_loadu2_m128 (hi, lo);
}
