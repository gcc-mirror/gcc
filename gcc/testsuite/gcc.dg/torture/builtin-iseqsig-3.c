/* { dg-do run { xfail powerpc*-*-* } } */
/* remove the xfail for powerpc when pr58684 is fixed */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fsignaling-nans" } */
/* { dg-require-effective-target fenv_exceptions_long_double } */

#include <fenv.h>

void
ftrue (long double x, long double y)
{
  if (!__builtin_iseqsig (x, y))
    __builtin_abort ();
}

void
ffalse (long double x, long double y)
{
  if (__builtin_iseqsig (x, y))
    __builtin_abort ();
}

int
main ()
{
  volatile long double f1, f2;

  f1 = 0.L; f2 = 0.f;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.L; f2 = -0.f;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.L; f2 = 1.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = -0.L; f2 = 1.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.L; f2 = __builtin_infl();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = -0.L; f2 = __builtin_infl();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.L; f2 = __builtin_nanl("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = -0.L; f2 = __builtin_nanl("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = 1.L; f2 = 1.f;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.L; f2 = 0.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.L; f2 = -0.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.L; f2 = __builtin_infl();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.L; f2 = __builtin_nanl("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_infl(); f2 = __builtin_infl();
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = __builtin_infl(); f2 = __builtin_nanl("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nanl(""); f2 = __builtin_nanl("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nansl(""); f2 = 1.L;
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = 1.L; f2 = __builtin_nansl("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nansl(""); f2 = __builtin_nansl("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  return 0;
}
