/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve_hw } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

#include "const_vector_zero_tail_9.c"

int
main (void)
{
  unsigned long n = svcntw ();
  uint32_t result[n];

  svst1_u32 (svptrue_b32 (), result, f ());

  for (unsigned long i = 0; i < n; ++i)
    {
      uint32_t expected = i < 1 ? 0x12345678 : 0;

      if (result[i] != expected)
	__builtin_abort ();
    }

  return 0;
}
