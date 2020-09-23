/* { dg-do run { target { movdir && { ! ia32 } } } } */
/* { dg-options "-mmovdiri -O2" } */

#include <x86intrin.h>
#include <cpuid.h>

unsigned long long int dest = -1LL;

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  if ((ecx & bit_MOVDIRI) == 0)
    return 0;

  _directstoreu_u64 (&dest, 0x12345678badbeef);

  if (dest != 0x12345678badbeef)
    abort ();

  return 0;
}
