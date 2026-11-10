/* { dg-do run { target { aarch64_lut_hw && le } } } */
/* { dg-options "-O2 -march=armv9-a+lut" } */

#include "perm-luti-2.c"

int
main (void)
{
  v8hi x = { 0, 1001, 2002, 3003, 4004, 5005, 6006, 7007 };
  v8hi y = { 8008, 9009, 10010, 11011, 12012, 13013, 14014, 15015 };
  v8hi a = luti4_h (x);
  v8hi b = luti4_h2 (x, y);
  v8hi c = (v8hi) luti4_hf2 ((v8hf) x, (v8hf) y);
  v8hi d = (v8hi) luti4_bf2 ((v8bf) x, (v8bf) y);
  for (unsigned int i = 0; i < 8; ++i)
    {
      unsigned short expected = i % 2 ? y[1] : x[2];
      if (a[i] != x[i % 2 ? 5 : 6]
	  || b[i] != expected || c[i] != expected || d[i] != expected)
	__builtin_abort ();
    }
  return 0;
}
