/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256d
__attribute__((noinline, unused))
test (double *e)
{
  return _mm256_loadu_pd (e);
}

void static
avx_test (void)
{
  union256d u;
  double e [4]  = {41124.234,2344.2354,8653.65635,856.43576};

  u.x = test (e);

  if (check_union256d (u, e))
    abort ();
}
