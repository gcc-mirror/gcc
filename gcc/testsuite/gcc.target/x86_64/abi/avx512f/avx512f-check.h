#include <stdlib.h>
#include "cpuid.h"

static void avx512f_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

#define DEBUG
  /* Run AVX test only if host has AVX support.  */
  if ((ecx & bit_OSXSAVE) == bit_OSXSAVE)
    {
      if (__get_cpuid_max (0, NULL) < 7)
	return 0;

      __cpuid_count (7, 0, eax, ebx, ecx, edx);

      if ((ebx & bit_AVX512F) == bit_AVX512F)
	{
	  avx512f_test ();
#ifdef DEBUG
	  __builtin_printf ("PASSED\n");
#endif
	}
#ifdef DEBUG
      else
	__builtin_printf ("SKIPPED\n");
#endif
    }
#ifdef DEBUG
  else
    __builtin_printf ("SKIPPED\n");
#endif

  return 0;
}
