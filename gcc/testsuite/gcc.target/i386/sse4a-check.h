#include <stdio.h>
#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void sse4a_test (void);

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_extended_cpuid_ecx ();

  /* Run SSE4a test only if host has SSE4a support.  */
  if ((cpu_facilities & bit_SSE4a))
    sse4a_test ();

  exit (0);
}
