/* { dg-do run { target { { aarch64_sve256_hw && aarch64_lut_hw } && le } } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=256" } */

#include "perm_luti_4.c"

int
main (void)
{
  v16hi x, y;
  for (unsigned int i = 0; i < 16; ++i)
    {
      x[i] = i * 1001 + 123;
      y[i] = i * 2001 + 456;
    }
  v16hi a = luti4_h2 (x, y);
  v16hi b = (v16hi) luti4_hf2 ((v16hf) x, (v16hf) y);
  v16hi c = (v16hi) luti4_bf2 ((v16bf) x, (v16bf) y);
  for (unsigned int i = 0; i < 16; ++i)
    {
      unsigned short expected = i % 2 ? y[1] : x[2];
      if (a[i] != expected || b[i] != expected || c[i] != expected)
	__builtin_abort ();
    }
  return 0;
}
