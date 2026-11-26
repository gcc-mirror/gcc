/* { dg-do run } */
/* { dg-require-effective-target bmi2 } */
/* { dg-options "-O2 -mbmi2 -mno-pclmul" } */

#include "bmi2-check.h"

#define main do_main

#include "../../c-c++-common/builtin-clmulg-4.c"

static void
bmi2_test (void)
{
  do_main ();
}
