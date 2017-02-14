#include <stdlib.h>
#include "cpuid.h"

static void adx_test (void);

static void __attribute__ ((noinline)) do_test (void)
{
  adx_test ();
}

  int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run ADX test only if host has ADX support.  */

  if (__get_cpuid_max (0, NULL) < 7)
    return 0;

  __cpuid_count (7, 0, eax, ebx, ecx, edx);

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

