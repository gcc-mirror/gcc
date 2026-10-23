/* PR middle-end/127785 */
/* Combining ORDERED and NE must not raise FE_INVALID for a quiet NaN.  */
/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions_double } */

#include <fenv.h>

#define NOIPA __attribute__ ((noipa))

/* Quiet & quiet: LTGT would signal.  */
NOIPA int ord_and_ne (double a, double b)
{ return !__builtin_isunordered (a, b) & (a != b); }

int
main (void)
{
  static const double as[4] = { __builtin_nan (""), 0.0, 1.0, 2.0 };
  /* Results for (NaN, 1), (0, 1), (1, 1) and (2, 1).  */
  static const int res[4] = { 0, 1, 0, 1 };

  for (unsigned j = 0; j < 4; j++)
    {
      feclearexcept (FE_INVALID);
      if (ord_and_ne (as[j], 1.0) != res[j])
	__builtin_abort ();
      if (fetestexcept (FE_INVALID))
	__builtin_abort ();
    }
  return 0;
}
