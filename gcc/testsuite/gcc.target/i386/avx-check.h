#include <stdlib.h>
#include "cpuid.h"
#include "m256-check.h"

static void avx_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  avx_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run AVX test only if host has AVX support.  */
  if ((ecx & (bit_AVX | bit_OSXSAVE)) == (bit_AVX | bit_OSXSAVE))
    {
      do_test ();
#ifdef DEBUG
      printf ("PASSED\n");
#endif
    }
#ifdef DEBUG
  else
    printf ("SKIPPED\n");
#endif

  return 0;
}
