/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve_hw } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

#include "const_vector_zero_tail_4.c"

int
main (void)
{
  unsigned long n = svcnth ();
  float16_t result[n];

  svst1_f16 (svptrue_b16 (), result, f ());

  for (unsigned long i = 0; i < n; ++i)
    {
      float16_t expected = i < 3 ? (i + 1) : 0;

      if (result[i] != expected)
	__builtin_abort ();
    }

  return 0;
}
