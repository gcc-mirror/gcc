#include <stdlib.h>

#include "cpuid.h"
#include "m128-check.h"

static void sse4_1_test (void);

#define MASK 0x2

static void
__attribute__ ((noinline))
do_test (void)
{
  sse4_1_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE4.1 test only if host has SSE4.1 support.  */
  if (ecx & bit_SSE4_1)
    do_test ();

  return 0;
}
