#ifdef DEBUG
#include <stdio.h>
#endif
#include <stdlib.h>
#include "cpuid.h"
#include "avx-os-support.h"

static void pclmul_avx_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  pclmul_avx_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run PCLMUL + AVX test only if host has PCLMUL + AVX support.  */
  if (((ecx & (bit_AVX | bit_OSXSAVE | bit_PCLMUL))
       == (bit_AVX | bit_OSXSAVE | bit_PCLMUL))
      && avx_os_support ())
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
