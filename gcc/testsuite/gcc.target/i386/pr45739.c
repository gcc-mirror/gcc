/* PR rtl-optimization/45739 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

__m128i var;

void
foo (void)
{
  __m128i zero = _mm_setzero_si128 ();
  var = _mm_xor_si128 (zero, var);
}

void
bar (void)
{
  __m128i zero = _mm_setzero_si128 ();
  var = _mm_or_si128 (var, zero);
}

/* { dg-final { scan-assembler-not "pxor\[^\n\]*xmm" } } */
/* { dg-final { scan-assembler-not "por\[^\n\]*xmm" } } */
