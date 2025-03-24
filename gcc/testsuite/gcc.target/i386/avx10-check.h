#include <stdlib.h>
#include "cpuid.h"
#include "m512-check.h"
#include "avx10-os-support.h"

#ifndef DO_TEST
#define DO_TEST do_test
#if defined(AVX10_512BIT) || defined(AVX10_SCALAR)
static void test_512 (void);
#else
static void test_256 (void);
static void test_128 (void);
#endif

__attribute__ ((noinline))
static void
do_test (void)
{
#if defined(AVX10_512BIT) || defined(AVX10_SCALAR)
  test_512 ();
#else
  test_256 ();
  test_128 ();
#endif
}
#endif

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
  /* Run AVX10 test only if host has ISA support.  */
  if (__builtin_cpu_supports ("avx10.1")
#ifdef AVX10_2
      && __builtin_cpu_supports ("avx10.2")
#endif
      && avx10_os_support ())
    {
      DO_TEST ();
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
