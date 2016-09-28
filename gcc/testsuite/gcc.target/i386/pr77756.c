/* { dg-do run }  */

#include "cpuid.h"

int
main ()
{
  __builtin_cpu_init ();

  if (__builtin_cpu_supports ("avx2"))
    {
      unsigned int eax, ebx, ecx, edx;

      if (!__get_cpuid (7, &eax, &ebx, &ecx, &edx))
	__builtin_abort ();

      if (!(ebx & bit_AVX2))
	__builtin_abort ();
    }

  return 0;
}
