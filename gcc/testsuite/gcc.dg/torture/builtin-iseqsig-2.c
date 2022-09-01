/* { dg-do run { xfail powerpc*-*-* } } */
/* remove the xfail for powerpc when pr58684 is fixed */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fsignaling-nans" } */
/* { dg-require-effective-target fenv_exceptions_double } */

#include <fenv.h>

void
ftrue (double x, double y)
{
  if (!__builtin_iseqsig (x, y))
    __builtin_abort ();
}

void
ffalse (double x, double y)
{
  if (__builtin_iseqsig (x, y))
    __builtin_abort ();
}

int
main ()
{
  volatile double f1, f2;

  f1 = 0.; f2 = 0.;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.; f2 = -0.;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.; f2 = 1.;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = -0.; f2 = 1.;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.; f2 = __builtin_inf();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = -0.; f2 = __builtin_inf();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.; f2 = __builtin_nan("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = -0.; f2 = __builtin_nan("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = 1.; f2 = 1.;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.; f2 = 0.;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.; f2 = -0.;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.; f2 = __builtin_inf();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.; f2 = __builtin_nan("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_inf(); f2 = __builtin_inf();
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = __builtin_inf(); f2 = __builtin_nan("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nan(""); f2 = __builtin_nan("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nans(""); f2 = 1.;
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = 1.; f2 = __builtin_nans("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nans(""); f2 = __builtin_nans("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  return 0;
}
