#include <stdlib.h>
#include "cpuid.h"
#ifdef DEBUG
#include <stdio.h>
#endif

static void adx_test (void);

static void __attribute__ ((noinline)) do_test (void)
{
  adx_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run ADX test only if host has ADX support.  */
  if (ebx & bit_ADX)
    {
      do_test ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
      return 0;
    }

#ifdef DEBUG
  printf ("SKIPPED\n");
#endif
  return 0;
}

