/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -msse -mfpmath=sse -mrecip" } */

#include "../../gcc.dg/i386-cpuid.h"

extern float sqrtf (float);
extern void abort (void);

#define N 8

int __attribute__((noinline))
main1 ()
{
  float a[N] = { 0.f, 18.f, 108.f, 324.f, 720.f, 1944.f, 3087.f, 5832.f };
  float b[N] = { 1.f, 2.f, 3.f, 4.f, 5.f, 6.f, 7.f, 8.f };
  float r[N];

  float rc[N] = { 0.f, 3.f, 6.f, 9.f, 12.f, 18.f, 21.f, 27.f };

  int i;

  for (i = 0; i < N; i++)
    {
      r[i] = sqrtf (a[i] / b[i]);
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (r[i] != rc[i])
	abort();
    }   

  return 0;
}

int
main ()
{
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if ((cpu_facilities & (bit_MMX | bit_SSE | bit_CMOV))
      != (bit_MMX | bit_SSE | bit_CMOV))
    /* If host has no vector support, pass.  */
    return 0;

  main1 ();
  return 0;
}
