/* { dg-skip-if "test SSE2 support" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O" } */
/* { dg-require-effective-target sse2 } */

#include "cpuid.h"

/* Test function argument passing.  PR target/15301.  */

extern void union_m128_1_x (void);
extern void exit (int);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE vector test only if host has SSE2 support.  */
  if (edx & bit_SSE2)
    union_m128_1_x ();

  exit (0);
}
