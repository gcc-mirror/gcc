/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmov(dqu|ups)\t" } } */
/* { dg-final { scan-assembler "\tvextract\[if]128\t" } } */

#include <immintrin.h>

void
foo (__m128i_u *hi, __m128i_u *lo, __m256i a)
{
  _mm256_storeu2_m128i (hi, lo, a);
}
