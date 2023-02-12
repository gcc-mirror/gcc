/* { dg-do run } */
/* { dg-options "-O2 -ftrapping-math" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions_double } */

#include <fenv.h>
#include <stdlib.h>

__attribute__ ((noipa)) double
x (void)
{
  double d = __builtin_inf ();
  return d / d;
}

int
main (void)
{
  double r = x ();
  if (!__builtin_isnan (r))
	abort ();
  if (!fetestexcept (FE_INVALID))
	abort ();
  exit (0);
}
