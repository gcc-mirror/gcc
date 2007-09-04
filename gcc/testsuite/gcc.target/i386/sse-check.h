#include <stdio.h>
#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void sse_test (void);

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_cpuid_edx ();

  /* Run SSE test only if host has SSE support.  */
  if ((cpu_facilities & bit_SSE))
    sse_test ();

  exit (0);
}
