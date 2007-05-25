#include <stdio.h>
#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void sse2_test (void);

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_cpuid_edx ();

  /* Run SSE2 test only if host has SSE2 support.  */
  if ((cpu_facilities & bit_SSE2))
    sse2_test ();

  exit (0);
}
