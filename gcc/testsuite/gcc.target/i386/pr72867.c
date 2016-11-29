/* PR target/72867 */
/* { dg-do run } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"
#include <xmmintrin.h>

static void
sse_test (void)
{
  float nan = __builtin_nanf ("");
  
  __m128 x = _mm_min_ps(_mm_set1_ps(nan), _mm_set1_ps(1.0f));

  if (x[0] != 1.0f)
    abort ();

  x = _mm_min_ps(_mm_set1_ps(1.f), _mm_set1_ps(nan));

  if (!__builtin_isnan (x[0]))
    abort ();
}
