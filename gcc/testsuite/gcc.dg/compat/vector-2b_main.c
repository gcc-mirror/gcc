/* { dg-skip-if "test AVX support" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target avx } */

/* Test compatibility of vector types: layout between separately-compiled
   modules, parameter passing, and function return.  This test uses
   vectors of floating points values.  */

#include "cpuid.h"

extern void vector_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run AVX vector test only if host has AVX support.  */
  if (ecx & bit_AVX)
    vector_2_x ();

  exit (0);
}
