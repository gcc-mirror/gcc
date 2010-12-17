#include <stdlib.h>
#include "m128-check.h"
#include "cpuid.h"
#include "sse-os-support.h"

static void sse_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE test only if host has SSE support.  */
  if ((edx & bit_SSE) && sse_os_support ())
    do_test ();

  return 0;
}
