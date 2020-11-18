/* Test DEC*_SNAN macros.  Test requiring runtime exceptions
   support.  */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions_dfp } */
/* { dg-options "-std=c2x" } */

#include <fenv.h>
#include <float.h>

volatile _Decimal32 d32 = DEC32_SNAN;
volatile _Decimal64 d64 = DEC64_SNAN;
volatile _Decimal128 d128 = DEC128_SNAN;

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
