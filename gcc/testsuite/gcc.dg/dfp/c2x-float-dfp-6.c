/* Test DEC_NAN macro.  Runtime exceptions test, to verify NaN is
   quiet not signaling.  (This would only actually fail for a
   signaling NaN in the hardware DFP case, because the software DFP
   support in libgcc does not integrate with hardware exceptions.)  */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-std=c2x" } */

#include <fenv.h>
#include <float.h>

#ifndef DEC_NAN
# error "DEC_NAN not defined"
#endif

volatile _Decimal32 d = DEC_NAN;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  d += d;
  if (fetestexcept (FE_INVALID))
    abort ();
  exit (0);
}
