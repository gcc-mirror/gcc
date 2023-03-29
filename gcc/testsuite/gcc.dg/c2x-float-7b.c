/* Test SNAN macros.  Runtime exceptions test, to verify NaN is
   signaling.  */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions_double } */
/* { dg-options "-std=c2x -pedantic-errors -fsignaling-nans" } */
/* { dg-add-options ieee } */

#include <fenv.h>
#include <float.h>

/* This should be defined if and only if signaling NaNs is supported
   for the given type.  If the testsuite gains effective-target
   support for targets not supporting signaling NaNs, this test
   should be made appropriately conditional.  */
#ifndef DBL_SNAN
#error "DBL_SNAN undefined"
#endif

volatile double d = DBL_SNAN;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  feclearexcept (FE_ALL_EXCEPT);
  d += d;
  if (!fetestexcept (FE_INVALID))
    abort ();
  exit (0);
}
