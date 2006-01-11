/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* N1150 5.4: Usual arithmetic conversions.
   C99 6.3.1.8[1] (New).

   Test arithmetic operators with different decimal float types, and
   between decimal float types and integer types.  */

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

volatile _Decimal32 d32a, d32b, d32c;
volatile _Decimal64 d64a, d64b, d64c;
volatile _Decimal128 d128a, d128b, d128c;
volatile int i;

void
init ()
{
  d32b = 123.456e94df;
  d64b = 12.3456789012345e383dd;
  d128b = 12345.6789012345678901e4000dl;

  d32c = 1.3df;
  d64c = 1.2dd;
  d128c = 1.1dl;

  i = 2;
}

int
main ()
{
  init ();

  /* Usual arithmetic conversions between decimal float types; addition.  */
  d128a = d128b + d32b;
  if (d128a < d128b)
    FAILURE
  d128a = d32b + d128b;
  if (d128a < d128b)
    FAILURE
  d128a = d128b + d64b;
  if (d128a < d128b)
    FAILURE
  d128a = d64b + d128b;
  if (d128a < d128b)
    FAILURE
  d64a = d64b + d32b;
  if (d64a < d64b)
    FAILURE
  d64a = d32b + d64b;
  if (d64a < d64b)
    FAILURE

  /* Usual arithmetic conversions between decimal float types;
     multiplication.  */
  d128a = d128b * d32c;
  if (d128a < d128b)
    FAILURE
  d128a = d32c * d128b;
  if (d128a < d128b)
    FAILURE
  d128a = d128b * d64c;
  if (d128a < d128b)
    FAILURE
  d128a = d64c * d128b;
  if (d128a < d128b)
    FAILURE
  d64a = d64b * d32c;
  if (d64a < d64b)
    FAILURE
  d64a = d32c * d64b;
  if (d64a < d64b)
    FAILURE

  /* Usual arithmetic conversions between decimal float and integer types.  */
  d32a = d32c + i;
  if (d32a != d32c + 2.0df)
    FAILURE
  d32a = d32c - i;
  if (d32a != d32c - 2.0df)
    FAILURE
  d32a = i * d32c;
  if (d32a != d32c + d32c)
    FAILURE
  d32a = d32c / i;
  if (d32a != d32c / 2.0df)
    FAILURE

  d64a = i + d64c;
  if (d64a != d64c + 2.0dd)
    FAILURE
  d64a = d64c - i;
  if (d64a != d64c - 2.0dd)
    FAILURE
  d64a = d64c * i;
  if (d64a != d64c + d64c)
    FAILURE
  d64a = d64c / i;
  if (d64a != d64c / 2.0dd)
    FAILURE

  d128a = d128c + i;
  if (d128a != d128c + 2.0dl)
    FAILURE
  d128a = d128c - i;
  if (d128a != d128c - 2.0dl)
    FAILURE
  d128a = i * d128c;
  if (d128a != d128c + d128c)
    FAILURE
  d128a = d128c / i;
  if (d128a != d128c / 2.0dl)
    FAILURE

  return 0;
}
