/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve_hw } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

#include "const_vector_zero_tail_8.c"

int
main (void)
{
  unsigned long n = svcntd ();
  float64_t result[n];

  svst1_f64 (svptrue_b64 (), result, f ());

  for (unsigned long i = 0; i < n; ++i)
    {
      float64_t expected = i < 1 ? 1.234375f : 0;

      if (result[i] != expected)
	__builtin_abort ();
    }

  return 0;
}
