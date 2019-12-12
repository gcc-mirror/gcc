/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmovdqu\t" } } */
/* { dg-final { scan-assembler "\tvinsert\[fi]128\t" } } */

#include <immintrin.h>

__m256i
foo (__m128i_u const *hi, __m128i_u const *lo)
{
  return _mm256_loadu2_m128i (hi, lo);
}
