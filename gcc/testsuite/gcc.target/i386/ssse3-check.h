#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void ssse3_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  ssse3_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSSE3 test only if host has SSSE3 support.  */
  if (ecx & bit_SSSE3)
    do_test ();

  return 0;
}
