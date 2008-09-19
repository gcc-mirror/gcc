/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -msse2 -mtune=nocona -mfpmath=387" { target { i?86-*-* x86_64-*-* } } } */

#ifdef __i386__
#include "i386-cpuid.h"
#endif

extern void abort (void);

int main(void)
{
  double arr[1000];
  double a, b;

  int i;

#ifdef __i386__
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid_edx ();
 
  /* Run SSE2 test only if host has SSE2 support.  */
  if (!(cpu_facilities & bit_SSE2))
    return 0;
#endif

  for (i = 0; i < 1000; i++)
    arr[i] = 4294967296.0 + (double)i;

  a = arr[0];
  b = (unsigned int)((unsigned long long int)a % 4294967296ULL);

  if (b >= 4294967296.0)
    abort ();

  return 0;
}
