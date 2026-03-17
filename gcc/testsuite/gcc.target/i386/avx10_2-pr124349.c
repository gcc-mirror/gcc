/* PR target/124349 */
/* { dg-do assemble { target { avx10_2 && masm_intel } } } */
/* { dg-options "-O2 -mavx10.2 -masm=intel" } */

#include <x86intrin.h>

int
foo (__m128bh v, __m128bh *p)
{
  return _mm_comilt_sbh (*p, v);
}

int
bar (__m128bh v, __m128bh w)
{
  return _mm_comilt_sbh (w, v);
}
