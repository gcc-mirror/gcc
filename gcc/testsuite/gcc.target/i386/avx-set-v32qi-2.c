/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256i
__attribute__((noinline))
foo (char x1, char x2, char x3, char x4,
     char x5, char x6, char x7, char x8,
     char x9, char x10, char x11, char x12,
     char x13, char x14, char x15, char x16,
     char x17, char x18, char x19, char x20,
     char x21, char x22, char x23, char x24,
     char x25, char x26, char x27, char x28,
     char x29, char x30, char x31, char x32)
{
  return _mm256_set_epi8 (x1, x2, x3, x4, x5, x6, x7, x8,
			  x9, x10, x11, x12, x13, x14, x15, x16,
			  x17, x18, x19, x20, x21, x22, x23, x24,
			  x25, x26, x27, x28, x29, x30, x31, x32);
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

  u.x = foo (v[31], v[30], v[29], v[28],
	     v[27], v[26], v[25], v[24],
	     v[23], v[22], v[21], v[20],
	     v[19], v[18], v[17], v[16],
	     v[15], v[14], v[13], v[12],
	     v[11], v[10], v[9], v[8],
	     v[7], v[6], v[5], v[4],
	     v[3], v[2], v[1], v[0]);
  if (check_union256i_b (u, v))
    abort ();
}
