/* { dg-do run { target { { aarch64_sve256_hw && aarch64_lut_hw } && le } } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=256" } */

#include "perm_luti_1.c"

int
main (void)
{
  v32qi x;
  v16hi y;
  for (unsigned int i = 0; i < 32; ++i)
    x[i] = i * 7 + 3;
  for (unsigned int i = 0; i < 16; ++i)
    y[i] = i * 1001 + 123;

  v32qi a = luti2_b (x);
  v32qi b = luti4_b (x);
  v16hi c = luti2_h (y);
  v16hi d = luti4_h (y);
  const unsigned char indices[4] = { FOUR };
  for (unsigned int i = 0; i < 32; ++i)
    if (a[i] != x[indices[i % 4]] || b[i] != x[i % 2 ? 9 : 10])
      __builtin_abort ();
  for (unsigned int i = 0; i < 16; ++i)
    if (c[i] != y[indices[i % 4]] || d[i] != y[i % 2 ? 9 : 10])
      __builtin_abort ();
  return 0;
}
