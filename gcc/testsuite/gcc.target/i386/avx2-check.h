#include <stdlib.h>
#include "cpuid.h"
#include "m256-check.h"
#include "avx-os-support.h"

static void avx2_test (void);

static void __attribute__ ((noinline)) do_test (void)
{
  avx2_test ();
}

static int
check_osxsave (void)
{
  unsigned int eax, ebx, ecx, edx;

  __cpuid (1, eax, ebx, ecx, edx);
  return (ecx & bit_OSXSAVE) != 0;
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid_count (7, 0, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run AVX2 test only if host has AVX2 support.  */
  if (check_osxsave () && (ebx & bit_AVX2) && avx_os_support ())
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
