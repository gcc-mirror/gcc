#include <stdlib.h>

#include "cpuid.h"

static void fma4_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  fma4_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run FMA4 test only if host has FMA4 support.  */
  if (ecx & bit_FMA4)
    do_test ();

  exit (0);
}
