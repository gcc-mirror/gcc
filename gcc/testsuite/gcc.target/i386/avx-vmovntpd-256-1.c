/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static void 
__attribute__((noinline))
test (double *p, __m256d s)
{
  return _mm256_stream_pd (p, s); 
}

static void
avx_test (void)
{
  union256d u;
  double e[4] __attribute__ ((aligned(32))) = {1,1,1,1};
   
  u.x = _mm256_set_pd (2134.3343, 1234.635654, -13443.35, 43.35345);
  test (e, u.x); 
   
  if (check_union256d (u, e))
    abort ();
}
