/* { dg-do compile } */
/* { dg-options "-O2 -msse4" } */
/* { dg-final { scan-assembler "ptest\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "pxor\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "pcmpeqb\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "pmovmskb\[ \\t\]" } } */

#include <smmintrin.h>

int is_zero(__m128i x)
{
  return _mm_movemask_epi8(_mm_cmpeq_epi8(x, _mm_setzero_si128())) == 0xffff;
}
