/* Excess precision tests.  Test excess precision of constants.  */
/* { dg-do run } */
/* { dg-options "-O2 -mfpmath=387 -fexcess-precision=standard" } */

#include <float.h>

extern void abort (void);
extern void exit (int);

volatile long double ldadd1 = 1.0l + 0x1.0p-30l;
volatile long double ld11f = 1.1f;
volatile long double ld11d = 1.1;
volatile long double ld11 = 1.1;

void
test_const (void)
{
  if (1.0f + 0x1.0p-30f != ldadd1)
    abort ();
  if (ld11f != ld11)
    abort ();
  if (ld11d != ld11)
    abort ();
  if (1.1f != ld11)
    abort ();
}

int
main (void)
{
  test_const ();
  exit (0);
}
