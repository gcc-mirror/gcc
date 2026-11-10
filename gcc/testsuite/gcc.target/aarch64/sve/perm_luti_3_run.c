/* { dg-do run { target { { aarch64_sve128_hw && aarch64_lut_hw } && le } } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=128" } */

#include "perm_luti_3.c"

int
main (void)
{
  v8hi x, y;
  for (unsigned int i = 0; i < 8; ++i)
    {
      x[i] = i * 1001 + 123;
      y[i] = i * 2001 + 456;
    }
  v8hi a = small_halfword_table (x);
  v8hi b = two_halfword_tables (x, y);
  for (unsigned int i = 0; i < 8; ++i)
    if (a[i] != x[i % 2 ? 6 : 7] || b[i] != (i % 2 ? y[1] : x[2]))
      __builtin_abort ();
  return 0;
}
