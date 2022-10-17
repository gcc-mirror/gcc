#include <stdio.h>
#include <stdlib.h>

#include "m128-check.h"
#include "cpuid.h"

static void sse4_2_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse4_2_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE4.2 test only if host has SSE4.2 support.  */
  if (ecx & bit_SSE4_2)
    do_test ();

  return 0;
}
