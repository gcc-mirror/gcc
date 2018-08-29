/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

static __m512i
__attribute__((noinline))
foo (char x1, char x2, char x3, char x4,
     char x5, char x6, char x7, char x8,
     char x9, char x10, char x11, char x12,
     char x13, char x14, char x15, char x16,
     char x17, char x18, char x19, char x20,
     char x21, char x22, char x23, char x24,
     char x25, char x26, char x27, char x28,
     char x29, char x30, char x31, char x32,
     char x33, char x34, char x35, char x36,
     char x37, char x38, char x39, char x40,
     char x41, char x42, char x43, char x44,
     char x45, char x46, char x47, char x48,
     char x49, char x50, char x51, char x52,
     char x53, char x54, char x55, char x56,
     char x57, char x58, char x59, char x60,
     char x61, char x62, char x63, char x64)
{
  return _mm512_set_epi8 (x1, x2, x3, x4, x5, x6, x7, x8,
			  x9, x10, x11, x12, x13, x14, x15, x16,
			  x17, x18, x19, x20, x21, x22, x23, x24,
			  x25, x26, x27, x28, x29, x30, x31, x32,
			  x33, x34, x35, x36, x37, x38, x39, x40,
			  x41, x42, x43, x44, x45, x46, x47, x48,
			  x49, x50, x51, x52, x53, x54, x55, x56,
			  x57, x58, x59, x60, x61, x62, x63, x64);
}

static void
avx512f_test (void)
{
  char v[64] =
    {
      -3, 60, 48, 104, -90, 37, -48, 78,
      4, 33, 81, 4, -89, 17, 8, 68,
      -13, 30, 78, 149, -70, -37, 98, 38,
      41, 73, 89, 14, 80, 117, 108, 8,
      12, 39, -8, -90, 17, 15, -26, 12,
      68, 8, 17, -88, 3, 80, 32, 3,
      38, 98, -37, -70, 149, 78, 30, -13,
      72, 88, 13, 79, 116, 107, 7, 42
    };
  union512i_b u;

  u.x = foo (v[63], v[62], v[61], v[60],
	     v[59], v[58], v[57], v[56],
	     v[55], v[54], v[53], v[52],
	     v[51], v[50], v[49], v[48],
	     v[47], v[46], v[45], v[44],
	     v[43], v[42], v[41], v[40],
	     v[39], v[38], v[37], v[36],
	     v[35], v[34], v[33], v[32],
	     v[31], v[30], v[29], v[28],
	     v[27], v[26], v[25], v[24],
	     v[23], v[22], v[21], v[20],
	     v[19], v[18], v[17], v[16],
	     v[15], v[14], v[13], v[12],
	     v[11], v[10], v[9], v[8],
	     v[7], v[6], v[5], v[4],
	     v[3], v[2], v[1], v[0]);
  if (check_union512i_b (u, v))
    abort ();
}
