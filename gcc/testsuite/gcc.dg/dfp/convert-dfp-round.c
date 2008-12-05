/* { dg-options "-O0 -std=gnu99" } */
/* { dg-skip-if "test is for emulation" { hard_dfp } { "*" } { "" } } */

/* N1150 5.2: Conversions among decimal floating types and between
   decimal floating types and generic floating types.
   C99 6.3.1.5(3) New.

   Perform conversions between DFP types in which the assigned value
   cannot be represented exactly in the result and must be rounded
   correctly according to the current rounding mode.

   Normally this would not be part of compiler testing, but conversions
   are currently handled in libgcc via decNumber.  */

#include "dfp-round.h"

extern void abort (void);
static int failcnt = 0;

/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#ifdef DBG
#include <stdio.h>
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

_Decimal32 d32;
_Decimal64 d64;
_Decimal128 d128;

_Decimal32 d64_to_d32 (_Decimal64 d) { return d; }
_Decimal64 d128_to_d64 (_Decimal128 d) { return d; }
_Decimal32 d128_to_d32 (_Decimal128 d) { return d; }

int
do_d64_to_d32 (_Decimal64 orig, _Decimal32 exp)
{
  d64 = orig;
  d32 = d64_to_d32 (d64);
  return (d32 == exp);
}

int
do_d128_to_d32 (_Decimal128 orig, _Decimal32 exp)
{
  d128 = orig;
  d32 = d128_to_d32 (d128);
  return (d32 == exp);
}

int
do_d128_to_d64 (_Decimal128 orig, _Decimal64 exp)
{
  d128 = orig;
  d64 = d128_to_d64 (d128);
  return (d64 == exp);
}

int
main ()
{
  DFP_SETROUND (FE_DEC_DOWNWARD);
  if (!do_d64_to_d32 (1.1111125dd, 1.111112df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111114df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111112df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111114df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111114dd)) FAILURE

  DFP_SETROUND (FE_DEC_TONEAREST);
  if (!do_d64_to_d32 (1.1111125dd, 1.111112df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111114df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111112df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111114df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111112df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111114df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111112df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111114df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111114dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111114dd)) FAILURE
  
  DFP_SETROUND (FE_DEC_TONEARESTFROMZERO);
  if (!do_d64_to_d32 (1.1111125dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111114df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111114df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111114df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111114df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111114dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111114dd)) FAILURE
  
  DFP_SETROUND (FE_DEC_TOWARDZERO);
  if (!do_d64_to_d32 (1.1111125dd, 1.111112df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111112df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111112df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111112df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111113df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111113dd)) FAILURE

  DFP_SETROUND (FE_DEC_UPWARD);
  if (!do_d64_to_d32 (1.1111125dd, 1.111113df)) FAILURE
  if (!do_d64_to_d32 (1.1111135dd, 1.111114df)) FAILURE
  if (!do_d64_to_d32 (-1.1111125dd, -1.111112df)) FAILURE
  if (!do_d64_to_d32 (-1.1111135dd, -1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111125dl, 1.111113df)) FAILURE
  if (!do_d128_to_d32 (1.1111135dl, 1.111114df)) FAILURE
  if (!do_d128_to_d32 (-1.1111125dl, -1.111112df)) FAILURE
  if (!do_d128_to_d32 (-1.1111135dl, -1.111113df)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111125dl, 1.111111111111113dd)) FAILURE
  if (!do_d128_to_d64 (1.1111111111111135dl, 1.111111111111114dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111125dl, -1.111111111111112dd)) FAILURE
  if (!do_d128_to_d64 (-1.1111111111111135dl, -1.111111111111113dd)) FAILURE
  
  if (failcnt)
    abort ();
  return 0;
}
