/* Test for unsafe floating-point conversions.  PR 36578.  */
/* { dg-do run } */
/* { dg-options "-msse2 -mfpmath=sse" } */
/* { dg-require-effective-target sse2 } */
/* { dg-require-effective-target large_long_double } */

#include "sse2-check.h"

extern void abort (void);
extern void exit (int);
extern int printf(const char *, ...);

volatile double d1 = 1.0;
volatile double d2 = 0x1.00001p-53;
volatile double d3;

static void
sse2_test (void)
{
  d3 = (double)((long double)d1 + (long double)d2);
  if (d3 != d1)
    abort ();
  exit (0);
}
