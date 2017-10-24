/* PR target/82460 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vbmi" } */
/* { dg-final { scan-assembler-not {\mvmovd} } } */

#include <x86intrin.h>

__m512i
f1 (__m512i x, __m512i y, char *z)
{
  return _mm512_permutex2var_epi32 (y, x, _mm512_loadu_si512 (z));
}

__m512i
f2 (__m512i x, __m512i y, char *z)
{
  return _mm512_permutex2var_epi32 (x, y, _mm512_loadu_si512 (z));
}

__m512i
f3 (__m512i x, __m512i y, __m512i z)
{
  return _mm512_permutex2var_epi8 (y, x, z);
}

__m512i
f4 (__m512i x, __m512i y, __m512i z)
{
  return _mm512_permutex2var_epi8 (x, y, z);
}
