#include <stdlib.h>

#include "cpuid.h"

static void sse5_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE5 test only if host has SSE5 support.  */
  if (ecx & bit_SSE5)
    sse5_test ();

  exit (0);
}
