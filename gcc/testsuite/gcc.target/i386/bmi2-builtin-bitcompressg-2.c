/* { dg-do run } */
/* { dg-require-effective-target bmi2 } */
/* { dg-options "-O2 -mbmi2 -mpopcnt" } */

#include "bmi2-check.h"

#define main do_main

#include "../../c-c++-common/builtin-bitcompressg-1.c"

static void
bmi2_test (void)
{
  unsigned int eax, ebx, ecx, edx;
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return;
  if (ecx & bit_POPCNT)
    do_main ();
}
