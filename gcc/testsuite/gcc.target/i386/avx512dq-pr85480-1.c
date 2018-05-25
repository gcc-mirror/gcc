/* PR target/85480 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-final { scan-assembler-times "vmovaps\[^\n\r]*xmm0\[^\n\r]*xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[^\n\r]*xmm0\[^\n\r]*xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[^\n\r]*xmm0\[^\n\r]*xmm0" 1 } } */

#include <x86intrin.h>

__m512
f1 (__m128 a)
{
  return _mm512_insertf32x4 (_mm512_set1_ps (0.0f), a, 0);
}

__m512d
f2 (__m128d a)
{
  return _mm512_insertf64x2 (_mm512_set1_pd (0.0), a, 0);
}

__m512i
f3 (__m128i a)
{
  return _mm512_inserti32x4 (_mm512_set1_epi32 (0), a, 0);
}
