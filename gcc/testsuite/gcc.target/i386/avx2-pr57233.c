/* PR tree-optimization/57233 */
/* { dg-do run { target avx2 } } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

int do_main (void);

static void
avx2_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr57233.c"
