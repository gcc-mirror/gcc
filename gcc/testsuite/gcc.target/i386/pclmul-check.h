#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void pclmul_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  pclmul_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run PCLMULQDQ test only if host has PCLMULQDQ support.  */
  if (ecx & bit_PCLMUL)
    {
      do_test ();
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
