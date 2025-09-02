/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-require-effective-target mmap } */
/* { dg-options "-Wno-experimental-fmv-target" } */

#include <cstdint>
#include <sys/auxv.h>

__attribute__((target_version ("default")))
int foo ()
{
  return 0;
}

__attribute__((target_version ("rng")))
int foo ()
{
  return 1;
}

__attribute__((target_version ("lse")))
int foo ()
{
  return 2;
}

typedef struct {
  uint64_t size;
  uint64_t hwcap;
  uint64_t hwcap2;
  uint64_t hwcap3;
  uint64_t hwcap4;
} ifunc_arg_t;

int impl ()
{
  return 0;
}

#ifndef _IFUNC_ARG_HWCAP
#define _IFUNC_ARG_HWCAP (1ULL << 62)
#endif

extern "C" void
__init_cpu_features_resolver (unsigned long hwcap, const ifunc_arg_t *arg);

extern "C" void *
fun_resolver (uint64_t a0, const ifunc_arg_t *a1)
{
  ifunc_arg_t arg = {};
  arg.size = sizeof (ifunc_arg_t);
  /* These flags determine that the implementation of foo ()
     that returns 2 will be selected.  */
  arg.hwcap = HWCAP_ATOMICS;
  arg.hwcap2 = HWCAP2_RNG;
  __init_cpu_features_resolver (arg.hwcap | _IFUNC_ARG_HWCAP, &arg);
  return (void *)(uintptr_t)impl;
}

extern "C" int fun (void) __attribute__((ifunc ("fun_resolver")));

/* In this test we expect that the manual resolver for the fun ()
   function will be executed before the automatic resolver for the
   FMV function foo ().  This is because resolvers from the same TU
   are executed according to the offset of corresponding relocations.

   Automatic resolver is generated in a dedicated section while the
   manually written resolver will be put in the .text section which
   will come first.

   The manual resolver above calls __init_cpu_features_resolver()
   supplying synthetic ifunc_arg_t fields that will determine the
   choice for the FMV implementation.
   */

int main ()
{
  int res = fun ();
  if (res == 0 && foo () == 2)
    return 0;
  return 1;
}
