/* PR target/85480 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mavx512dq -mavx512vl" } */
/* { dg-final { scan-assembler-times "vmovaps\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]" 1 } } */
/* { dg-final { scan-assembler-times "vmovapd\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa32\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]" 1 } } */

#include <x86intrin.h>

__m512
f1 (__m128 a)
{
  register __m128 b __asm ("xmm16");
  asm ("" : "=v" (b) : "0" (a));
  register __m512 c __asm ("xmm17") = _mm512_insertf32x4 (_mm512_set1_ps (0.0f), b, 0);
  asm ("" : "+v" (c));
  return c;
}

__m512d
f2 (__m128d a)
{
  register __m128d b __asm ("xmm16");
  asm ("" : "=v" (b) : "0" (a));
  register __m512d c __asm ("xmm17") = _mm512_insertf64x2 (_mm512_set1_pd (0.0), b, 0);
  asm ("" : "+v" (c));
  return c;
}

__m512i
f3 (__m128i a)
{
  register __m128i b __asm ("xmm16");
  asm ("" : "=v" (b) : "0" (a));
  register __m512i c __asm ("xmm17") = _mm512_inserti32x4 (_mm512_set1_epi32 (0), b, 0);
  asm ("" : "+v" (c));
  return c;
}
