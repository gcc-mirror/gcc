/* PR rtl-optimization/87918 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <x86intrin.h>

__m128 b, c, d;

void
foo (float f)
{
  c = _mm_set_ss (f);
  d = _mm_cmple_ss (c, b);
}
