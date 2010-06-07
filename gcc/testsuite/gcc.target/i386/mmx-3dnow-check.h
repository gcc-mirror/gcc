#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

static void mmx_3dnow_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  mmx_3dnow_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (0x80000001, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run 3DNow! test only if host has 3DNow! support.  */
  if (edx & bit_3DNOW)
    do_test ();

  return 0;
}
