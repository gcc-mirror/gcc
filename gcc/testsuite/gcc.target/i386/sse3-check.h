#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void sse3_test (void);

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_cpuid_ecx ();

  /* Run SSE3 test only if host has SSE3 support.  */
  if ((cpu_facilities & bit_SSE3))
    sse3_test ();

  exit (0);
}
