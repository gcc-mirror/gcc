#include <stdlib.h>
#include "cpuid.h"
#ifdef DEBUG
#include <stdio.h>
#endif

static void rtm_test (void);

static void __attribute__ ((noinline)) do_test (void)
{
  rtm_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run RTM test only if host has RTM support.  */
  if (ebx & bit_RTM)
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
