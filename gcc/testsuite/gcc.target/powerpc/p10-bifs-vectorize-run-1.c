/* { dg-do run } */
/* The checked bifs are only supported on 64-bit env.  */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model" } */

#include "p10-bifs-vectorize-1.h"

/* Check if vectorized built-in functions run expectedly.  */

/* Make optimize (1) to avoid vectorization applied on check func.  */

#define CHECK(name)                                                           \
  __attribute__ ((optimize (1))) void check_##name ()                         \
  {                                                                           \
    test_##name ();                                                           \
    for (int i = 0; i < N; i++)                                               \
      {                                                                       \
	uLL exp = __builtin_##name (ull_a[i], ull_b[i]);                      \
	if (exp != ull_c[i])                                                  \
	  __builtin_abort ();                                                 \
      }                                                                       \
  }

CHECK (cfuged)
CHECK (cntlzdm)
CHECK (cnttzdm)
CHECK (pdepd)
CHECK (pextd)

int
main ()
{
  for (int i = 0; i < N; i++)
    {
      ull_a[i] = 0x789a * (i * 11 - 5) - 0xcd1 * (i * 5 - 7);
      ull_b[i] = 0xfedc * (i * 7 + 3) + 0x467 * (i * 7 - 11);
    }

  check_cfuged ();
  check_cntlzdm ();
  check_cnttzdm ();
  check_pdepd ();
  check_pextd ();

  return 0;
}

