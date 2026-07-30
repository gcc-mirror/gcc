/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve_hw } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

#include "const_vector_zero_tail_6.c"

int
main (void)
{
  unsigned long n = svcntb ();
  uint8_t result[n];

  svst1_u8 (svptrue_b8 (), result, f ());

  for (unsigned long i = 0; i < n; ++i)
    {
      uint8_t expected = i < 1 ? 1 : 0;

      if (result[i] != expected)
	__builtin_abort ();
    }

  return 0;
}
