/* { dg-do run } */
/* { dg-require-effective-target bmi2 } */
/* { dg-options "-O2 -mbmi2 -mno-popcnt" } */

#include "bmi2-check.h"

#define main do_main

#include "../../c-c++-common/builtin-bitcompressg-1.c"

static void
bmi2_test (void)
{
  do_main ();
}
