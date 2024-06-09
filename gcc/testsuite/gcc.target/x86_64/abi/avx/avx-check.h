#include <stdlib.h>
#include "cpuid.h"

static void avx_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run AVX test only if host has AVX support.  */
  if ((ecx & (bit_AVX | bit_OSXSAVE)) == (bit_AVX | bit_OSXSAVE))
    {
      avx_test ();
#ifdef DEBUG
      __builtin_printf ("PASSED\n");
#endif
    }
#ifdef DEBUG
  else
    __builtin_printf ("SKIPPED\n");
#endif

  return 0;
}
