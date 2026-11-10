/* { dg-do run { target { aarch64_sve512_hw && aarch64_sve2p3_hw } } } */
/* { dg-options "-O2 -march=armv9-a+sve2p3 -msve-vector-bits=512" } */

#include "perm_luti_6.c"

int
main (void)
{
  v32hi x, y;
  for (unsigned int i = 0; i < 32; ++i)
    {
      x[i] = i * 1001 + 123;
      y[i] = i * 2001 + 456;
    }
  v32hi a = luti6_h (x, y);
  v32hi b = (v32hi) luti6_hf ((v32hf) x, (v32hf) y);
  v32hi c = (v32hi) luti6_bf ((v32bf) x, (v32bf) y);
  for (unsigned int i = 0; i < 32; ++i)
    {
      unsigned int index = (i % 2 == 0 ? i
			    : (2 * i + (i < 16 ? 1 : -1)) % 32);
      unsigned short expected = i % 2 == 0 ? x[index] : y[index];
      if (a[i] != expected || b[i] != expected || c[i] != expected)
	__builtin_abort ();
    }
  return 0;
}
