#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void sse4a_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse4a_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE4a test only if host has SSE4a support.  */
  if (ecx & bit_SSE4a)
    do_test ();

  return 0;
}
