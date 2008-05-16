#include <stdlib.h>
#include "cpuid.h"
#include "m128-check.h"

static void sse2_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE2 test only if host has SSE2 support.  */
  if (edx & bit_SSE2)
    sse2_test ();

  return 0;
}
