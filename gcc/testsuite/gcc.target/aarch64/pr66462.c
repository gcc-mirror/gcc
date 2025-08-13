/* { dg-do run } */
/* { dg-options "-O2 -fsignaling-nans -fno-inline" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

static void t_inff (float x, bool res)
{
  if (__builtin_isinff (x) != res)
    __builtin_abort ();
  if (__builtin_isinff (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

static void t_inf (double x, bool res)
{
  if (__builtin_isinf (x) != res)
    __builtin_abort ();
  if (__builtin_isinf (-x) != res)
    __builtin_abort ();
  if (fetestexcept (FE_INVALID))
    __builtin_abort ();
}

int
main ()
{
  feclearexcept (FE_INVALID);

  t_inff (0.0f, 0);
  t_inff (1.0f, 0);
  t_inff (__builtin_inff (), 1);
  t_inff (__builtin_nansf (""), 0);
  t_inff (__builtin_nanf (""), 0);

  t_inf (0.0, 0);
  t_inf (1.0, 0);
  t_inf (__builtin_inf (), 1);
  t_inf (__builtin_nans (""), 0);
  t_inf (__builtin_nan (""), 0);

  return 0;
}
