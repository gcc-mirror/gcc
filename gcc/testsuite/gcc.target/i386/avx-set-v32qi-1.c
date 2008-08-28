/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (char *v)
{
  return _mm256_set_epi8 (v[31], v[30], v[29], v[28],
			  v[27], v[26], v[25], v[24],
			  v[23], v[22], v[21], v[20],
			  v[19], v[18], v[17], v[16],
			  v[15], v[14], v[13], v[12],
			  v[11], v[10], v[9], v[8],
			  v[7], v[6], v[5], v[4],
			  v[3], v[2], v[1], v[0]);
}

static void
avx_test (void)
{
  char v[32] =
    { 
      -3, 60, 48, 104, -90, 37, -48, 78,
      4, 33, 81, 4, -89, 17, 8, 68,
      -13, 30, 78, 149, -70, -37, 98, 38,
      41, 73, 89, 14, 80, 117, 108, 8
    };
  union256i_b u;

  u.x = foo (v);
  if (check_union256i_b (u, v))
    abort ();
}
