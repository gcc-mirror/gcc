/* Test DEC*_SNAN macros defined in <float.h> with DFP support.  */
/* { dg-do run } */
/* { dg-options "-std=c2x" } */

#include <float.h>

#ifndef DEC32_SNAN
# error "DEC32_SNAN not defined"
#endif

#ifndef DEC64_SNAN
# error "DEC64_SNAN not defined"
#endif

#ifndef DEC128_SNAN
# error "DEC128_SNAN not defined"
#endif

volatile _Decimal32 d32 = DEC32_SNAN;
volatile _Decimal64 d64 = DEC64_SNAN;
volatile _Decimal128 d128 = DEC128_SNAN;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  (void) _Generic (DEC32_SNAN, _Decimal32 : 0);
  if (!__builtin_isnan (DEC32_SNAN))
    abort ();
  if (!__builtin_isnan (d32))
    abort ();
  (void) _Generic (DEC64_SNAN, _Decimal64 : 0);
  if (!__builtin_isnan (DEC64_SNAN))
    abort ();
  if (!__builtin_isnan (d64))
    abort ();
  (void) _Generic (DEC128_SNAN, _Decimal128 : 0);
  if (!__builtin_isnan (DEC128_SNAN))
    abort ();
  if (!__builtin_isnan (d128))
    abort ();
  exit (0);
}
