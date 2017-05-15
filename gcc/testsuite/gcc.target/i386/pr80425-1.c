/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mtune=intel" } */

#include <x86intrin.h>

__m512i
f1 (__m512i x, int a)
{
  return _mm512_srai_epi32 (x, a);
}

/* { dg-final { scan-assembler-times "movd\[ \\t\]+\[^\n\]*%xmm" 1 } } */
