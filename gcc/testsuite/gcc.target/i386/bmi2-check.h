#include <stdio.h>
#include <stdlib.h>
#include "cpuid.h"

static void bmi2_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  bmi2_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run BMI2 test only if host has BMI2 support.  */
  if (ebx & bit_BMI2)
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
