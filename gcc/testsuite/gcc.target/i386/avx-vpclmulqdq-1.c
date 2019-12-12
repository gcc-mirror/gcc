/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mvpclmulqdq" } */

#include <x86intrin.h>

__m256i
foo (__m256i x, __m256i y)
{
  return _mm256_clmulepi64_epi128 (x, y, 0);
}
