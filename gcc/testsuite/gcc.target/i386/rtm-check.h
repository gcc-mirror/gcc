#include <stdlib.h>
#include "cpuid.h"

static void rtm_test (void);

static void __attribute__ ((noinline)) do_test (void)
{
  rtm_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (__get_cpuid_max (0, NULL) >= 7)
    {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);
      if (ebx & bit_RTM)
	{
	  do_test ();
#ifdef DEBUG
	  printf ("PASSED\n");
#endif
	  return 0;
	}
    }
#ifdef DEBUG
  printf ("SKIPPED\n");
#endif
  return 0;
}
