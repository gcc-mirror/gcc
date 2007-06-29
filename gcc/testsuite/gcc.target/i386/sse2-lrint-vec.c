/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse2" } */

#include "../../gcc.dg/i386-cpuid.h"

extern long lrint (double);
extern void abort (void);

#define N 32

int __attribute__((noinline))
main1 ()
{
  double a[N] = {0.4,3.5,6.6,9.4,12.5,15.6,18.4,21.5,24.6,27.4,30.5,33.6,36.4,39.5,42.6,45.4,0.5,3.6,6.4,9.5,12.6,15.4,18.5,21.6,24.4,27.5,30.6,33.4,36.5,39.6,42.4,45.5};
  long r[N];

  int i;

  for (i = 0; i < N; i++)
    {
      r[i] = lrint (a[i]);
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (r[i] != lrint (a[i]))
	abort();
    }   

  return 0;
}

int
main ()
{
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if ((cpu_facilities & (bit_MMX | bit_SSE | bit_SSE2 | bit_CMOV))
      != (bit_MMX | bit_SSE | bit_SSE2 | bit_CMOV))
    /* If host has no vector support, pass.  */
    return 0;

  main1 ();
  return 0;
}
