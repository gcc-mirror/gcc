/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpandd\[ \\t\]+\\(%(?:eax|rdi|edi)\\)\\\{1to\[1-8\]+\\\}, %xmm\[0-9\]+, %xmm0" 1 } } */
/* { dg-final { scan-assembler-not "vpbroadcastd\[^\n\]*%xmm\[0-9\]+" } } */

#include <immintrin.h>

__m128i
foo (__m128i x, int *f)
{
  return (__m128i) ((__v4su) x & (__v4su) _mm_set1_epi32 (*f));
}
