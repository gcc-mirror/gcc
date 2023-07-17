#include <stdlib.h>
#include "cpuid.h"
#include "m256-check.h"
#include "avx-os-support.h"

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
  if (((ecx & (bit_AVX | bit_OSXSAVE)) == (bit_AVX | bit_OSXSAVE))
      && avx_os_support ()
#ifdef AVXIFMA
      && __builtin_cpu_supports ("avxifma")
#endif
#ifdef AVXVNNIINT8
      && __builtin_cpu_supports ("avxvnniint8")
#endif
#ifdef AVXNECONVERT
      && __builtin_cpu_supports ("avxneconvert")
#endif
#ifdef AVXVNNIINT16
      && __builtin_cpu_supports ("avxvnniint16")
#endif
      )
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
