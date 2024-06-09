/* Test DEC_INFINITY defined in <float.h> with DFP support.  */
/* { dg-options "-std=c23" } */

#include <float.h>

#ifndef DEC_INFINITY
# error "DEC_INFINITY not defined"
#endif

volatile _Decimal32 d = DEC_INFINITY;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  (void) _Generic (DEC_INFINITY, _Decimal32 : 0);
  if (!(DEC_INFINITY > DEC32_MAX))
    abort ();
  if (!(d > DEC32_MAX))
    abort ();
  exit (0);
}
