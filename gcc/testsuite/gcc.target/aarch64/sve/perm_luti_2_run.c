/* { dg-do run { target { aarch64_sve256_hw && aarch64_sve2p3_hw } } } */
/* { dg-options "-O2 -march=armv9-a+sve2p3 -msve-vector-bits=256" } */

#include "perm_luti_2.c"

int
main (void)
{
  v32qi x, y;
  for (unsigned int i = 0; i < 32; ++i)
    {
      x[i] = i;
      y[i] = i + 100;
    }
  v32qi z = luti6_b (x, y);
  for (unsigned int i = 0; i < 32; ++i)
    {
      unsigned int index = (i % 2 == 0 ? i
			    : (2 * i + (i < 16 ? 1 : -1)) % 32);
      if (z[i] != (i % 2 == 0 ? x[index] : y[index]))
	__builtin_abort ();
    }
  return 0;
}
