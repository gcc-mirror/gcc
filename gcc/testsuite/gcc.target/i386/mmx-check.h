#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void mmx_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run MMX test only if host has MMX support.  */
  if (edx & bit_MMX)
    mmx_test ();

  return 0;
}
