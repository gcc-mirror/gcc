#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "cpuid.h"
#include "mpx-os-support.h"

static int
__attribute__ ((noinline))
mpx_test (int, const char **);

#ifdef SHOULDFAIL
#define NORUNRES 1
#else
#define NORUNRES 0
#endif

#define DEBUG

static int
check_osxsave (void)
{
  unsigned int eax, ebx, ecx, edx;

  __cpuid (1, eax, ebx, ecx, edx);
  return (ecx & bit_OSXSAVE) != 0;
}

int
main (int argc, const char **argv)
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return NORUNRES;

  /* Run MPX test only if host has MPX support.  */
  if (check_osxsave () && (ebx & bit_MPX) && mpx_os_support ())
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
