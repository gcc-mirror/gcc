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

static int
check_osxsave (void)
{
  unsigned int eax, ebx, ecx, edx;

  __cpuid (1, eax, ebx, ecx, edx);
  return (ecx & bit_OSXSAVE) != 0;
}

__attribute__((noipa,target("no-avx")))
int
avx512_runtime_support_p ()
{
  /* Run AVX512 test only if host has ISA support.  */
  if (__builtin_cpu_supports ("avx512f")
#ifdef AVX512VL
      && __builtin_cpu_supports ("avx512vl")
#endif
#ifdef AVX512ER
      && __builtin_cpu_supports ("avx512er")
#endif
#ifdef AVX512CD
      && __builtin_cpu_supports ("avx512cd")
#endif
#ifdef AVX512DQ
      && __builtin_cpu_supports ("avx512dq")
#endif
#ifdef AVX512BW
      && __builtin_cpu_supports ("avx512bw")
#endif
#ifdef AVX512IFMA
      && __builtin_cpu_supports ("avx512ifma")
#endif
#ifdef AVX512VBMI
      && __builtin_cpu_supports ("avx512vbmi")
#endif
#ifdef AVX5124FMAPS
      && __builtin_cpu_supports ("avx5124fmaps")
#endif
#ifdef AVX5124VNNIW
      && __builtin_cpu_supports ("avx5124vnniw")
#endif
#ifdef AVX512VPOPCNTDQ
      && __builtin_cpu_supports ("avx512vpopcntdq")
#endif
#ifdef AVX512BITALG
      && __builtin_cpu_supports ("avx512bitalg")
#endif
#ifdef GFNI
      && __builtin_cpu_supports ("gfni")
#endif
#ifdef AVX512VBMI2
      && __builtin_cpu_supports ("avx512vbmi2")
#endif
#ifdef AVX512VNNI
      && __builtin_cpu_supports ("avx512vnni")
#endif
#ifdef AVX512FP16
      && __builtin_cpu_supports ("avx512fp16")
#endif
#ifdef VAES
      && __builtin_cpu_supports ("vaes")
#endif
#ifdef VPCLMULQDQ
      && __builtin_cpu_supports ("vpclmulqdq")
#endif
#ifdef AVX512VP2INTERSECT
      && __builtin_cpu_supports ("avx512vp2intersect")
#endif
      )
    {
      return 1;
    }

  return 0;
}

int
main ()
{
  if (avx512_runtime_support_p ())
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
