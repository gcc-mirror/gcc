/* { dg-do run } */
/* { dg-require-effective-target bmi } */
/* { dg-options "-O2 -mbmi" } */

#include <x86intrin.h>

#include "bmi-check.h"

int
__attribute__((noinline, noclone))
foo (int x)
{
  return __tzcnt_u32 (x) & 0x1f;
}

static void
bmi_test ()
{
  if (foo (0) != 0)
    abort ();
}
