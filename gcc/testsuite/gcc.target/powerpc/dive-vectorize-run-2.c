/* { dg-do run } */
/* The checked bifs are only supported on 64-bit env.  */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model" } */

#include "dive-vectorize-2.h"

/* Check if test cases with signed/unsigned int extended division
   vectorization run successfully.  */

/* Make optimize (1) to avoid vectorization applied on check func.  */

__attribute__ ((optimize (1))) void
check_divde ()
{
  test_divde ();
  for (int i = 0; i < N; i++)
    {
      sLL exp = __builtin_divde (sll_a[i], sll_b[i]);
      if (exp != sll_c[i])
	__builtin_abort ();
    }
}

__attribute__ ((optimize (1))) void
check_divdeu ()
{
  test_divdeu ();
  for (int i = 0; i < N; i++)
    {
      uLL exp = __builtin_divdeu (ull_a[i], ull_b[i]);
      if (exp != ull_c[i])
	__builtin_abort ();
    }
}

int
main ()
{
  for (int i = 0; i < N; i++)
    {
      sll_a[i] = 0x102 * (i * 3 + 2);
      sll_b[i] = 0x789ab * (i * 3 + 1);
      ull_a[i] = 0x2345 * (i * 11 + 3) - 0xcd1 * (i * 5 - 7);
      ull_b[i] = 0x6078e * (i * 7 + 3) + 0xefa * (i * 7 - 11);
      if (sll_b[i] == 0 || ull_b[i] == 0)
	__builtin_abort ();
    }

  check_divde ();
  check_divdeu ();

  return 0;
}

