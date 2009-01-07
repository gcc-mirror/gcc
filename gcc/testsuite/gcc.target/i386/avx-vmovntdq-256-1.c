/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static void
__attribute__((noinline))
test (__m256i *p, __m256i s)
{
  return _mm256_stream_si256 (p, s); 
}

static void
avx_test (void)
{
  union256i_d u;
  int e[8] __attribute__ ((aligned(32))) = {1,1,1,1,1,1,1,1};

  u.x = _mm256_set_epi32 (2434, 6845, 3789, 4683,
			  4623, 2236, 8295, 1084);

  test ((__m256i *)e, u.x);
  
  if (check_union256i_d (u, e))
    abort ();
}
