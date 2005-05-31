/* Execution test for argument passing with SSE2 and local functions
   Written by Paolo Bonzini, 25 January 2005 */

/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */
#include <assert.h>
#include "i386-cpuid.h"

static float xs (void)
{
  return 3.14159265;
}

float ys (float a)
{
  return xs () * a;
}

static double xd (void)
{
  return 3.1415926535;
}

double yd (double a)
{
  return xd () * a;
}

int main()
{
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if (cpu_facilities & bit_SSE2)
    {
      assert (ys (1) == xs ());
      assert (ys (2) == xs () * 2);
      assert (yd (1) == xd ());
      assert (yd (2) == xd () * 2);
    }
  return 0;
}
