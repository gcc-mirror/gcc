#include <stdio.h>
#include <stdlib.h>
#include "cpuid.h"
#include "sse-os-support.h"

static void sse3_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse3_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;
 
  /* Run SSE3 test only if host has SSE3 support.  */
  if ((ecx & bit_SSE3) && sse_os_support ())
    do_test ();

  return 0;
}
