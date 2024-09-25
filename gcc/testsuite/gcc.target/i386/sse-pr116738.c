/* PR target/116738 */
/* { dg-do run } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

static inline float
clamp (float f)
{
  __m128 v = _mm_set_ss (f);
  __m128 zero = _mm_setzero_ps ();
  __m128 greatest = _mm_set_ss (__FLT_MAX__);
  v = _mm_min_ss (v, greatest);
  v = _mm_max_ss (v, zero);
  return _mm_cvtss_f32 (v);
}

static void
sse_test (void)
{
  float f = clamp (-0.0f);
  if (f != 0.0f || __builtin_signbitf (f))
    abort ();
  f = clamp (__builtin_nanf (""));
  if (__builtin_isnanf (f) || f != __FLT_MAX__)
    abort ();
}
