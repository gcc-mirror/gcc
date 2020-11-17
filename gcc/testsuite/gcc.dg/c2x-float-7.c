/* Test SNAN macros.  Runtime exceptions test, to verify NaN is
   signaling.  */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-std=c2x -pedantic-errors -fsignaling-nans" } */
/* { dg-add-options ieee } */

#include <fenv.h>
#include <float.h>

/* These should be defined if and only if signaling NaNs are supported
   for the given types.  If the testsuite gains effective-target
   support for targets not supporting signaling NaNs, or not
   supporting them for all types, this test should be made
   appropriately conditional.  */
#ifndef FLT_SNAN
#error "FLT_SNAN undefined"
#endif
#ifndef DBL_SNAN
#error "DBL_SNAN undefined"
#endif
#ifndef LDBL_SNAN
#error "LDBL_SNAN undefined"
#endif

volatile float f = FLT_SNAN;
volatile double d = DBL_SNAN;
volatile long double ld = LDBL_SNAN;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  feclearexcept (FE_ALL_EXCEPT);
  f += f;
  if (!fetestexcept (FE_INVALID))
    abort ();
  feclearexcept (FE_ALL_EXCEPT);
  d += d;
  if (!fetestexcept (FE_INVALID))
    abort ();
  feclearexcept (FE_ALL_EXCEPT);
  ld += ld;
  if (!fetestexcept (FE_INVALID))
    abort ();
  exit (0);
}
