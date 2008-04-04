#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void aes_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run AES test only if host has AES support.  */
  if (ecx & bit_AES)
    {
      aes_test ();
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
