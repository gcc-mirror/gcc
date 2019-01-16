/* PR target/53759 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

#include <xmmintrin.h>

void
foo (__m128 *x, __m64 *y)
{
  __m128 a = _mm_add_ps (x[1], x[2]);
  __m128 b = _mm_loadl_pi (a, y);
  *x = _mm_add_ps (b, b);
}

/* { dg-final { scan-assembler "vmovlps\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vshufps\[ \\t\]" } } */
