/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static void 
__attribute__((noinline))
test (float *p, __m256 s)
{
  return _mm256_stream_ps (p, s); 
}

static void
avx_test (void)
{
  union256 u;
  float e[8] __attribute__ ((aligned(32)));

  u.x = _mm256_set_ps (24.43, 68.346, -43.35, 546.46,
		       46.9, -2.78, 82.9, -0.4);
  test (e, u.x); 
  
  if (check_union256 (u, e))
    abort ();
}
