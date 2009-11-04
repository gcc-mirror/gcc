#include <stdlib.h>

#include "cpuid.h"

static void xop_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run XOP test only if host has XOP support.  */
  if (ecx & bit_XOP)
    xop_test ();

  exit (0);
}
