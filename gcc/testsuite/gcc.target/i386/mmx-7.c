/* PR middle-end/26379 */
/* { dg-do compile } */
/* { dg-options "-O2 -mmmx" } */

#include <mmintrin.h>

void
foo (__m64 *p)
{
  __m64 m;

  m = p[0];
  m = _mm_srli_pi16(m, 2);
  m = _mm_slli_pi16(m, 8);

  p[0] = m;
  _mm_empty();
}
