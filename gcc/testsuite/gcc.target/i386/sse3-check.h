#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void sse3_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;
 
  /* Run SSE3 test only if host has SSE3 support.  */
  if (ecx & bit_SSE3)
    sse3_test ();

  return 0;
}
