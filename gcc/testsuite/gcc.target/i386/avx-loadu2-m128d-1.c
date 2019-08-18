/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmovupd\t" } } */
/* { dg-final { scan-assembler "\tvinsertf128\t" } } */

#include <immintrin.h>

__m256d
foo (double const *hi, double const *lo)
{
  return _mm256_loadu2_m128d (hi, lo);
}
