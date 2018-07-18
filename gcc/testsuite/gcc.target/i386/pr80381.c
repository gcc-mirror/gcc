/* PR target/80381 */
/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */

#include <x86intrin.h>

__m512i
f1 (__m512i x, int a)
{
  return _mm512_srai_epi32 (x, a);
}

__m512i
f2 (__m512i x, __m512i y, __mmask16 m, int a)
{
  return _mm512_mask_srai_epi32 (y, m, x, a);
}

__m512i
f3 (__m512i x)
{
  return _mm512_srai_epi32 (x, 6);
}

__m512i
f4 (__m512i x, __m512i y, __mmask16 m)
{
  return _mm512_mask_srai_epi32 (y, m, x, 6);
}
