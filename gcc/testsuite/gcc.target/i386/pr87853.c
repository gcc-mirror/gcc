/* PR target/87853 */
/* { dg-do compile } */
/* { dg-options "-O2 -funsigned-char -msse2 -mno-sse3 -masm=att" } */
/* { dg-final { scan-assembler-times "\tpcmpgtb\t%xmm" 2 } } */
/* { dg-final { scan-assembler-not "\tpsubusb\t" } } */
/* { dg-final { scan-assembler-not "\tpcmpeqb\t" } } */

#include <x86intrin.h>

__m128i
foo (__m128i x, __m128i y)
{
  return _mm_cmpgt_epi8 (x, y);
}

__m128i
bar (__m128i x, __m128i y)
{
  return _mm_cmplt_epi8 (x, y);
}
