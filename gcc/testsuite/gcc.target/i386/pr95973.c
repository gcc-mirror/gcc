/* { dg-do run } */
/* { dg-options "-O2 -Wall" } */

#include <cpuid.h>
#include <cpuid.h>

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
  int cpuid_info[4];

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  __cpuidex (cpuid_info, 7, 0);

  if (cpuid_info[0] != eax
      || cpuid_info[1] != ebx
      || cpuid_info[2] != ecx
      || cpuid_info[3] != edx)
    __builtin_abort ();

  return 0;
}
