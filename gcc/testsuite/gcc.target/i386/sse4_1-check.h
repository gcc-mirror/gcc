#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void sse4_1_test (void);

#define MASK 0x2

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_cpuid_ecx ();

  /* Run SSE4.1 test only if host has SSE4.1 support.  */
  if ((cpu_facilities & bit_SSE4_1))
    sse4_1_test ();

  exit (0);
}
