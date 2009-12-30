#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void mmx_3dnow_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run 3DNow! test only if host has 3DNow! support.  */
  if (edx & bit_3DNOW)
    mmx_3dnow_test ();

  return 0;
}
