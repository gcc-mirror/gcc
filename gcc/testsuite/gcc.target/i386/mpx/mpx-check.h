#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "cpuid.h"

static int
__attribute__ ((noinline))
mpx_test (int, const char **);

#ifdef SHOULDFAIL
#define NORUNRES 1
#else
#define NORUNRES 0
#endif

#define DEBUG

int
main (int argc, const char **argv)
{
  unsigned int eax, ebx, ecx, edx;

  if (__get_cpuid_max (0, NULL) < 7)
    return 0;

  __cpuid_count (7, 0, eax, ebx, ecx, edx);

  /* Run MPX test only if host has MPX support.  */
  if (ebx & bit_MPX)
    mpx_test (argc, argv);
  else
    {
#ifdef DEBUG
      printf ("SKIPPED\n");
#endif
      return NORUNRES;
    }

  return 0;
}
