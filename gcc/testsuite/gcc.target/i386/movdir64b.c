/* { dg-do run { target movdir } } */
/* { dg-options "-mmovdir64b -O2" } */

#include <x86intrin.h>
#include <cpuid.h>
#include <string.h>

unsigned long long int src[8] = {1, 2, 3, 4, 5, 6, 7, 8};
unsigned long long int dest[8] __attribute__ ((aligned (64)))
  = {-1, -1, -1, -1, -1, -1, -1, -1};

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  if ((ecx & bit_MOVDIR64B) == 0)
    return 0;

  _movdir64b (dest, src);

  if (memcmp (dest, src, sizeof (dest)) != 0)
    abort ();

  return 0;
}
