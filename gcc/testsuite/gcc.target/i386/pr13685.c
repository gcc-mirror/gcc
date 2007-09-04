/* PR target/13685 */
/* { dg-do run } */
/* { dg-options "-Os -msse" } */

#include "sse-check.h"

#include <xmmintrin.h>

void foo (__m128 *, __m64 *, int);

__m128 xmm0 = { 0 };
__m64 mm0 = { 0 };

static void
sse_test (void)
{
  foo (&xmm0, &mm0, 4);
}

void
foo (__m128 *dst, __m64 *src, int n)
{
  __m128 xmm0 = { 0 };
  while (n > 64)
    {
      puts ("");
      xmm0 = _mm_cvtpi32_ps (xmm0, *src);
      *dst = xmm0;
      n--;
    }
}
