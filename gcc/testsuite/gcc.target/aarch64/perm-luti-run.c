/* { dg-do run { target { aarch64_lut_hw && le } } } */
/* { dg-options "-O2 -march=armv9-a+lut" } */

#include "perm-luti.c"

int
main (void)
{
  v16qi x = { 0, 17, 34, 51, 68, 85, 102, 119,
	      136, 153, 170, 187, 204, 221, 238, 255 };
  v8hi y = { 0, 1001, 2002, 3003, 4004, 5005, 6006, 7007 };
  v16qi a = luti2_b (x);
  v16qi b = luti4_b (x);
  v8hi c = luti2_h (y);
  const unsigned char indices[4] = { 2, 0, 3, 1 };
  for (unsigned int i = 0; i < 16; ++i)
    if (a[i] != x[indices[i % 4]] || b[i] != x[i % 2 ? 9 : 10])
      __builtin_abort ();
  for (unsigned int i = 0; i < 8; ++i)
    if (c[i] != y[indices[i % 4]])
      __builtin_abort ();
  return 0;
}
