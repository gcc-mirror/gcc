/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__ ((noinline, unused))
test (__m256i *p)
{
  return _mm256_load_si256 (p);
}

void static
avx_test (void)
{
  union256i_d u;
  int e [8] __attribute__ ((aligned (32))) = {23, 67, 53, 6, 4, 6, 85, 234};

  u.x = test ((__m256i *)e);

  if (check_union256i_d (u, e))
    abort ();
}
