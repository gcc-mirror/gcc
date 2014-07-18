/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

#include <x86intrin.h>

__m512i zmm;
__m128i xmm;

void test (void)
{
  xmm = _mm512_extracti32x4_epi32 (zmm, 0);
}
