#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void mmx_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  mmx_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run MMX test only if host has MMX support.  */
  if (edx & bit_MMX)
    do_test ();

  return 0;
}
