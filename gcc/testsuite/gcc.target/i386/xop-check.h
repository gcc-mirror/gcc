#include <stdlib.h>

#include "cpuid.h"
#include "m256-check.h"

static void xop_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  xop_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run XOP test only if host has XOP support.  */
  if (ecx & bit_XOP)
    do_test ();

  exit (0);
}
