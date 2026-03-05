/* PR target/124349 */
/* { dg-do assemble { target { avx10_2 && masm_intel } } } */
/* { dg-options "-O2 -mavx10.2 -masm=intel" } */

#include <x86intrin.h>

__m128h
foo (__m128i *p)
{
  return _mm_cvthf8_ph (*p);
}

__m128h
bar (__m128i *p, __m128h w, __mmask8 u)
{
  return _mm_mask_cvthf8_ph (w, u, *p);
}

__m256h
baz (__m128i *p)
{
  return _mm256_cvthf8_ph (*p);
}

__m256h
qux (__m128i *p, __m256h w, __mmask16 u)
{
  return _mm256_mask_cvthf8_ph (w, u, *p);
}

__m512h
fred (__m256i *p)
{
  return _mm512_cvthf8_ph (*p);
}

__m512h
corge (__m256i *p, __m512h w, __mmask32 u)
{
  return _mm512_mask_cvthf8_ph (w, u, *p);
}
