#include <stdlib.h>
#include "cpuid.h"
#include "m512-check.h"
#include "avx512f-os-support.h"

#ifndef DO_TEST
#define DO_TEST do_test
#ifdef AVX512VL
static void test_256 (void);
static void test_128 (void);
#else
static void test_512 (void);
#endif

__attribute__ ((noinline))
static void
do_test (void)
{
#ifdef AVX512VL
  test_256 ();
  test_128 ();
#else
  test_512 ();
#endif
}
#endif

int
main ()
{
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    goto skipped;

  /* Run AVX512F test only if host has AVX512F support.  */
  if (!(ecx & bit_OSXSAVE))
    goto skipped;

  if (__get_cpuid_max (0, NULL) < 7)
    goto skipped;

  __cpuid_count (7, 0, eax, ebx, ecx, edx);

  if (!(ebx & bit_AVX512F))
    goto skipped;

#ifdef AVX512VL
  if (!(ebx & bit_AVX512VL))
    goto skipped;
#endif

#ifdef AVX512ER
  if (!(ebx & bit_AVX512ER))
    goto skipped;
#endif

#ifdef AVX512CD
  if (!(ebx & bit_AVX512CD))
    goto skipped;
#endif

#ifdef AVX512DQ
  if (!(ebx & bit_AVX512DQ))
    goto skipped;
#endif

#ifdef AVX512BW
  if (!(ebx & bit_AVX512BW))
    goto skipped;
#endif

#ifdef AVX512IFMA
  if (!(ebx & bit_AVX512IFMA))
    goto skipped;
#endif

#ifdef AVX512VBMI
  if (!(ecx & bit_AVX512VBMI))
    goto skipped;
#endif

#ifdef AVX5124FMAPS
  if (!(edx & bit_AVX5124FMAPS))
    goto skipped;
#endif

#ifdef AVX5124VNNIW
  if (!(edx & bit_AVX5124VNNIW))
    goto skipped;
#endif

#ifdef AVX512VPOPCNTDQ
  if (!(ecx & bit_AVX512VPOPCNTDQ))
    goto skipped;
#endif

  if (!avx512f_os_support ())
    goto skipped;

  DO_TEST ();

#ifdef DEBUG
  printf ("PASSED\n");
#endif
  return 0;

skipped:
#ifdef DEBUG
  printf ("SKIPPED\n");
#endif
  return 0;
}
