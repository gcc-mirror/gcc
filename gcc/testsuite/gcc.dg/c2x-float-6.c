/* Test SNAN macros.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -pedantic-errors -fsignaling-nans" } */
/* { dg-add-options ieee } */

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
  (void) _Generic (FLT_SNAN, float : 0);
  (void) _Generic (DBL_SNAN, double : 0);
  (void) _Generic (LDBL_SNAN, long double : 0);
  if (!__builtin_isnan (FLT_SNAN))
    abort ();
  if (!__builtin_isnan (f))
    abort ();
  if (!__builtin_isnan (DBL_SNAN))
    abort ();
  if (!__builtin_isnan (d))
    abort ();
  if (!__builtin_isnan (LDBL_SNAN))
    abort ();
  if (!__builtin_isnan (ld))
    abort ();
  exit (0);
}
