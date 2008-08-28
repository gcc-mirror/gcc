/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (long long *v)
{
  return _mm256_set_epi64x (v[3], v[2], v[1], v[0]);
}

static void
avx_test (void)
{
  long long v[4]
    = { 0x12e9e94645ad8LL, 0x851c0b39446LL,
	0x786784645245LL, 0x9487731234LL };
  union256i_q u;

  u.x = foo (v);
  if (check_union256i_q (u, v))
    abort ();
}
