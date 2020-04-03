/* { dg-do run { xfail powerpc*-*-* } } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-skip-if "fenv" { powerpc-ibm-aix* } } */

#include <fenv.h>

int
__attribute__ ((noinline, noclone))
f1 (float a, float b)
{
  return -__builtin_isgreater (a, b);
}

int
__attribute__ ((noinline, noclone))
f2 (float a, float b)
{
  return -(a > b);
}

int
main (void)
{
  volatile int r;

  float nanf = __builtin_nanf ("");
  float argf = 1.0f;

  feclearexcept (FE_INVALID);

  r = f1 (nanf, argf);
  if (r != 0 || fetestexcept (FE_INVALID))
    __builtin_abort ();

  r = f2 (nanf, argf);
  if (r != 0 || !fetestexcept (FE_INVALID))
    __builtin_abort ();

  return 0;
}
