/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (short x1, short x2, short x3, short x4,
     short x5, short x6, short x7, short x8,
     short x9, short x10, short x11, short x12,
     short x13, short x14, short x15, short x16)
{
  return _mm256_set_epi16 (x1, x2, x3, x4, x5, x6, x7, x8,
			   x9, x10, x11, x12, x13, x14, x15, x16);
}

static void
avx_test (void)
{
  short v[16] =
    { 
      -3, 60, 48, 104, -90, 37, -48, 78,
      4, 33, 81, 4, -89, 17, 8, 68
    };
  union256i_w u;

  u.x = foo (v[15], v[14], v[13], v[12],
	     v[11], v[10], v[9], v[8],
	     v[7], v[6], v[5], v[4],
	     v[3], v[2], v[1], v[0]);
  if (check_union256i_w (u, v))
    abort ();
}
