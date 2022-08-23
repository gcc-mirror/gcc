/* PR target/105079 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse4.1" } */
/* { dg-final { scan-assembler-not "pextrw" } } */

#include <xmmintrin.h>

void store16 (void *p, __m128i v)
{
  _mm_storeu_si16 (p, v);
}
