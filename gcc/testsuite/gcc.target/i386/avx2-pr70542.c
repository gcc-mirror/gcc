/* PR tree-optimization/70542 */
/* { dg-do run } */
/* { dg-options "-O3 -mavx2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

#define main() do_main ()

#include "../../gcc.dg/torture/pr70542.c"

static void
avx2_test (void)
{
  do_main ();
}
