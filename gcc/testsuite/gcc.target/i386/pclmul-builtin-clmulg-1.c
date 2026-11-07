/* { dg-do run } */
/* { dg-require-effective-target pclmul } */
/* { dg-options "-O2 -mpclmul" } */

#ifndef CHECK_H
#define CHECK_H "pclmul-check.h"
#endif

#include CHECK_H

#define main do_main

#include "../../c-c++-common/builtin-clmulg-1.c"

static void
pclmul_test (void)
{
  do_main ();
}
