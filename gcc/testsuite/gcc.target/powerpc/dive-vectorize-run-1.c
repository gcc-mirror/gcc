/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model" } */

#include "dive-vectorize-1.h"

/* Check if test cases with signed/unsigned int extended division
   vectorization run successfully.  */

/* Make optimize (1) to avoid vectorization applied on check func.  */

__attribute__ ((optimize (1))) void
check_divwe ()
{
  test_divwe ();
  for (int i = 0; i < N; i++)
    {
      si exp = __builtin_divwe (si_a[i], si_b[i]);
      if (exp != si_c[i])
	__builtin_abort ();
    }
}

__attribute__ ((optimize (1))) void
check_divweu ()
{
  test_divweu ();
  for (int i = 0; i < N; i++)
    {
      ui exp = __builtin_divweu (ui_a[i], ui_b[i]);
      if (exp != ui_c[i])
	__builtin_abort ();
    }
}

int
main ()
{
  for (int i = 0; i < N; i++)
    {
      si_a[i] = 0x10 * (i * 3 + 2);
      si_b[i] = 0x7890 * (i * 3 + 1);
      ui_a[i] = 0x234 * (i * 11 + 3) - 0xcd * (i * 5 - 7);
      ui_b[i] = 0x6078 * (i * 7 + 3) + 0xef * (i * 7 - 11);
      if (si_b[i] == 0 || ui_b[i] == 0)
	__builtin_abort ();
    }

  check_divwe ();
  check_divweu ();

  return 0;
}

