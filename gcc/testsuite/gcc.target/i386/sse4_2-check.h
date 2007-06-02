#include <stdio.h>
#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void sse4_2_test (void);

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_cpuid_ecx ();

  /* Run SSE4.2 test only if host has SSE4.2 support.  */
  if ((cpu_facilities & bit_SSE4_2))
    sse4_2_test ();

  exit (0);
}
