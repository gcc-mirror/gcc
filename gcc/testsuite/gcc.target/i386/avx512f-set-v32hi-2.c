/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

static __m512i
__attribute__((noinline))
foo (short x1, short x2, short x3, short x4,
     short x5, short x6, short x7, short x8,
     short x9, short x10, short x11, short x12,
     short x13, short x14, short x15, short x16,
     short x17, short x18, short x19, short x20,
     short x21, short x22, short x23, short x24,
     short x25, short x26, short x27, short x28,
     short x29, short x30, short x31, short x32)
{
  return _mm512_set_epi16 (x1, x2, x3, x4, x5, x6, x7, x8,
			   x9, x10, x11, x12, x13, x14, x15, x16,
			   x17, x18, x19, x20, x21, x22, x23, x24,
			   x25, x26, x27, x28, x29, x30, x31, x32);
}

static void
avx512f_test (void)
{
  short v[32] =
    {
      -3, 60, 48, 16383, -90, 37, -48, 78,
      4, 33, 81, 4, -89, -32768, 8, 68,
      -13, 30, 78, 149, -70, -37, 98, 38,
      41, 73, 89, 14, 80, 117, 108, 8
    };
  union512i_w u;

  u.x = foo (v[31], v[30], v[29], v[28],
	     v[27], v[26], v[25], v[24],
	     v[23], v[22], v[21], v[20],
	     v[19], v[18], v[17], v[16],
	     v[15], v[14], v[13], v[12],
	     v[11], v[10], v[9], v[8],
	     v[7], v[6], v[5], v[4],
	     v[3], v[2], v[1], v[0]);
  if (check_union512i_w (u, v))
    abort ();
}
