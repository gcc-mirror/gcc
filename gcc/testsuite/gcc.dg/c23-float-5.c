/* Test NAN macro.  Runtime exceptions test, to verify NaN is quiet
   not signaling.  */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options ieee } */

#include <fenv.h>
#include <float.h>

/* This should be defined if and only if quiet NaNs are supported for
   type float.  If the testsuite gains effective-target support for
   targets not supporting NaNs, or not supporting them for all types,
   this test should only be run for targets supporting quiet NaNs for
   float.  */
#ifndef NAN
#error "NAN undefined"
#endif

volatile float f = NAN;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  f += f;
  if (fetestexcept (FE_INVALID))
    abort ();
  exit (0);
}
