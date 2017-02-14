/* { dg-do run } */
/* { dg-options "-O3 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include "avx512f-check.h"

#define main do_main

#include "../../gcc.c-torture/execute/pr78675.c"

static void
avx512f_test (void)
{
  do_main ();
}
