/* PR tree-optimization/57233 */
/* { dg-do run { target avx } } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static void
avx_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr57233.c"
