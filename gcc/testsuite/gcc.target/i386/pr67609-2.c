/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#include <emmintrin.h>

__m128d reg = { 2.0, 4.0 };

void
__attribute__((noinline))
set_lower (double b)
{
  double v[2];
  _mm_store_pd(v, reg);
  v[0] = b;
  reg = _mm_load_pd(v);
}

static void
sse2_test (void)
{
  set_lower (6.0);

  if (reg[1] != 4.0)
    __builtin_abort ();
}
