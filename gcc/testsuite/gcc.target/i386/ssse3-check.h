#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void ssse3_test (void);

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_cpuid_ecx ();

  /* Run SSSE3 test only if host has SSSE3 support.  */
  if ((cpu_facilities & bit_SSSE3))
    ssse3_test ();

  exit (0);
}
