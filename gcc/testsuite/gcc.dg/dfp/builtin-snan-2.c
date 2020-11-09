/* Test __builtin_nansd* functions.  Test requiring runtime exceptions
   support.  */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions_dfp } */
/* { dg-options "" } */

#include <fenv.h>

volatile _Decimal32 d32 = __builtin_nansd32 ("");
volatile _Decimal64 d64 = __builtin_nansd64 ("");
volatile _Decimal128 d128 = __builtin_nansd128 ("");

extern void abort (void);
extern void exit (int);

int
main (void)
{
  feclearexcept (FE_ALL_EXCEPT);
  d32 += d32;
  if (!fetestexcept (FE_INVALID))
    abort ();
  feclearexcept (FE_ALL_EXCEPT);
  d32 += d32;
  if (fetestexcept (FE_INVALID))
    abort ();
  feclearexcept (FE_ALL_EXCEPT);
  d64 += d64;
  if (!fetestexcept (FE_INVALID))
    abort ();
  feclearexcept (FE_ALL_EXCEPT);
  d64 += d64;
  if (fetestexcept (FE_INVALID))
    abort ();
  feclearexcept (FE_ALL_EXCEPT);
  d128 += d128;
  if (!fetestexcept (FE_INVALID))
    abort ();
  feclearexcept (FE_ALL_EXCEPT);
  d128 += d128;
  if (fetestexcept (FE_INVALID))
    abort ();
  exit (0);
}
