/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (int *v)
{
  return _mm256_set_epi32 (v[7], v[6], v[5], v[4],
			   v[3], v[2], v[1], v[0]);
}

static void
avx_test (void)
{
  int v[8]
    = { 19832468, 6576856, 8723467, 234566,
	786784, 645245, 948773, 1234 };
  union256i_d u;

  u.x = foo (v);
  if (check_union256i_d (u, v))
    abort ();
}
