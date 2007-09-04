#include <stdio.h>
#include <stdlib.h>

#include "../../gcc.dg/i386-cpuid.h"

static void mmx_test (void);

int
main ()
{
  unsigned long cpu_facilities;
 
  cpu_facilities = i386_cpuid_edx ();

  /* Run MMX test only if host has MMX support.  */
  if ((cpu_facilities & bit_MMX))
    mmx_test ();

  exit (0);
}
