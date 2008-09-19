/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -msse2 -mtune=core2 -mfpmath=387" { target { i?86-*-* x86_64-*-* } } } */

#ifdef __i386__
#include "cpuid.h"
#endif

extern void abort (void);

int main(void)
{
  double arr[1000];
  double a, b;

  int i;

#ifdef __i386__
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE2 test only if host has SSE2 support.  */
  if (!(edx & bit_SSE2))
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
