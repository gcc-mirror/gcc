/* { dg-options "-std=gnu99" } */

/* N1150 5.2 Conversions among decimal floating types and between
   decimal floating types and generic floating types.
   C99 6.3.1.5(4) Conversions, arithmetic operands, real floating types.  */

/* Long double isn't supported yet at runtime, so disable those checks.  */
#define SKIP_LONG_DOUBLE

extern void abort (void);
static int failcnt;

/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#ifdef DBG
#include <stdio.h>
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

volatile _Decimal32 d32;
volatile _Decimal64 d64;
volatile _Decimal128 d128;
volatile float sf;
volatile double df;
#ifndef SKIP_LONG_DOUBLE
volatile long double tf;
#endif

int
main ()
{
  /* Conversions from decimal float to binary float. */

  /* Conversions from _Decimal32. */
  d32 = 2.0df;
  sf = d32;
  if (sf != 2.0f)
    FAILURE

  df = d32;
  if (df != 2.0)
    FAILURE

#ifndef SKIP_LONG_DOUBLE
  tf = d32;
  if (tf != 2.0l)
    FAILURE
#endif

  /* Conversions from _Decimal64. */
  d64 = -7.0dd;
  sf = d64;
  if (sf != -7.0f)
    FAILURE
  
  df = d64;
  if (df != -7.0)
    FAILURE

#ifndef SKIP_LONG_DOUBLE
  tf = d64;
  if (tf != -7.0l)
    FAILURE
#endif

  /* Conversions from _Decimal128. */
  d128 = 30.0dl;
  sf = d128;
  if (sf != 30.0f)
    FAILURE

  df = d128;
  if (df != 30.0)
    FAILURE

  df = d128;
  if (df != 30.0l)
    FAILURE

  /* Conversions from binary float to decimal float. */
  sf = 30.0f;
  d32 = sf;
  if (d32 != 30.0df)
    FAILURE

  d64 = sf;
  if (d64 != 30.0dd)
    FAILURE

  df = -2.0;
  d32 = df;
  if (d32 != -2.0df)
    FAILURE

  d64 = df;
  if (d64 != -2.0dd)
    FAILURE

  d128 = df;
  if (d128 != -2.0dl)
    FAILURE
  
  sf = 30.0f;
  d128 = sf;
  if (d128 != 30.0dl)
    FAILURE

#ifndef SKIP_LONG_DOUBLE
  tf = -22.0l;
  d32 = tf;
  if (d32 != -22.0df)
    FAILURE

  d64 = tf;
  if (d64 != -22.0dd)
    FAILURE

  d128 = tf;
  if (d128 != -22.0dl)
    FAILURE
#endif

  /* 2**(-11) = 0.00048828125. */
  d128 = 0.000488281251dl;
  sf = d128;
  if (sf != 0.00048828125f)
    FAILURE
  /* 2**(-25) = 0.298023223876953125E-7.  */
  d128 = 2.98023223876953125E-8dl;
  df = d128;
  if (df < (2.9802322387695312e-08 - 0.00000000001)
      || df > (2.9802322387695312e-08 + 0.00000000001))
    FAILURE

  if (failcnt != 0)
    abort ();

  return 0;
}
