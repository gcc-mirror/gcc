/* { dg-do run { xfail powerpc*-*-* } } */
/* remove the xfail for powerpc when pr58684 is fixed */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fsignaling-nans" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

void
ftrue (float x, float y)
{
  if (!__builtin_iseqsig (x, y))
    __builtin_abort ();
}

void
ffalse (float x, float y)
{
  if (__builtin_iseqsig (x, y))
    __builtin_abort ();
}

int
main ()
{
  volatile float f1, f2;

  f1 = 0.f; f2 = 0.f;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.f; f2 = -0.f;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.f; f2 = 1.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = -0.f; f2 = 1.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.f; f2 = __builtin_inff();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = -0.f; f2 = __builtin_inff();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 0.f; f2 = __builtin_nanf("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = -0.f; f2 = __builtin_nanf("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = 1.f; f2 = 1.f;
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.f; f2 = 0.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.f; f2 = -0.f;
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.f; f2 = __builtin_inff();
  ffalse (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = 1.f; f2 = __builtin_nanf("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_inff(); f2 = __builtin_inff();
  ftrue (f1, f2);
  if (fetestexcept (FE_INVALID)) __builtin_abort ();

  f1 = __builtin_inff(); f2 = __builtin_nanf("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nanf(""); f2 = __builtin_nanf("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nansf(""); f2 = 1.f;
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = 1.f; f2 = __builtin_nansf("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  f1 = __builtin_nansf(""); f2 = __builtin_nansf("");
  ffalse (f1, f2);
  if (!fetestexcept (FE_INVALID)) __builtin_abort ();
  feclearexcept (FE_INVALID);

  return 0;
}
