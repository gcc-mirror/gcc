#include <stdlib.h>
#include <stdio.h>
#include "cpuid.h"
#include "m256-check.h"

static void f16c_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run F16C test only if host has F16C support.  */
  if (ecx & bit_F16C)
    {
      f16c_test ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
    }
#ifdef DEBUG
  else
    printf ("SKIPPED\n");
#endif

  return 0;
}
