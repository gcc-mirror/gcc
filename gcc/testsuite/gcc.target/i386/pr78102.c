/* PR target/78102 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse4.2 -msse4.1" } */
/* { dg-final { scan-assembler-times "pcmpeqq" 3 } } */

#include <x86intrin.h>

__m128i
foo (const __m128i x, const __m128i y)
{
  return _mm_cmpeq_epi64 (x, y);
}

__v2di
bar (const __v2di x, const __v2di y)
{
  return x == y;
}

__v2di
baz (const __v2di x, const __v2di y)
{
  return x != y;
}
