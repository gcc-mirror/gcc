/* { dg-do run { target movdir } } */
/* { dg-options "-mmovdiri -O2" } */

#include <x86intrin.h>
#include <cpuid.h>

unsigned int dest = -1;

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  if ((ecx & bit_MOVDIRI) == 0)
    return 0;

  _directstoreu_u32 (&dest, 0xbadbeef);

  if (dest != 0xbadbeef)
    abort ();

  return 0;
}
