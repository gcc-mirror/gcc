/* Test NAN macro.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-add-options ieee } */

#include <float.h>

/* This should be defined if and only if quiet NaNs are supported for
   type float.  If the testsuite gains effective-target support for
   targets not supporting NaNs, or not supporting them for all types,
   this test should be split into versions for targets with and
   without NaNs for float.  */
#ifndef NAN
#error "NAN undefined"
#endif

volatile float f = NAN;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  (void) _Generic (NAN, float : 0);
  if (!__builtin_isnan (NAN))
    abort ();
  if (!__builtin_isnan (f))
    abort ();
  if (!__builtin_isnan (f + f))
    abort ();
  exit (0);
}
