/* { dg-options "-std=gnu99" } */

/* N1150 4: Characteristics of decimal floating types <float.h>.
   C99 5.2.4.2.2a[3]: New.

   Verify constants about range of decimal float and three components of
   decimal float defined in float.h.  */

/* Make sure we are exporting the right values to float.h. */
#ifndef	__STDC_WANT_DEC_FP__
#define __STDC_WANT_DEC_FP__ 1
#endif

#include <float.h>

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

int main ()
{
  if (DEC32_MANT_DIG != 7) FAILURE
  if (DEC64_MANT_DIG != 16) FAILURE
  if (DEC128_MANT_DIG != 34) FAILURE

  if (DEC32_MIN_EXP != -94) FAILURE
  if (DEC64_MIN_EXP != -382) FAILURE
  if (DEC128_MIN_EXP != -6142) FAILURE

  if (DEC32_MAX_EXP != 97) FAILURE
  if (DEC64_MAX_EXP != 385) FAILURE
  if (DEC128_MAX_EXP != 6145) FAILURE

  if (DEC32_MAX != 9.999999E96DF) FAILURE
  if (DEC64_MAX != 9.999999999999999E384DD) FAILURE
  if (DEC128_MAX != 9.999999999999999999999999999999999E6144DL) FAILURE

  if (DEC32_EPSILON != 1E-6DF) FAILURE
  if (DEC64_EPSILON != 1E-15DD) FAILURE
  if (DEC128_EPSILON != 1E-33DL) FAILURE
  
  if (DEC32_MIN != 1E-95DF) FAILURE
  if (DEC64_MIN != 1E-383DD) FAILURE
  if (DEC128_MIN != 1E-6143DL) FAILURE

  if (DEC32_SUBNORMAL_MIN != 0.000001E-95DF) FAILURE
  if (DEC64_SUBNORMAL_MIN != 0.000000000000001E-383DD) FAILURE
  if (DEC128_SUBNORMAL_MIN != 0.000000000000000000000000000000001E-6143DL)
    FAILURE

  if (failcnt != 0)
    abort ();

  return 0;
}
