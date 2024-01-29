/* Test DEC_NAN defined in <float.h> with DFP support.  */
/* { dg-options "-std=c23" } */

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
  (void) _Generic (DEC_NAN, _Decimal32 : 0);
  if (!__builtin_isnan (DEC_NAN))
    abort ();
  if (!__builtin_isnan (d))
    abort ();
  exit (0);
}
