/* PR tree-optimization/57233 */
/* { dg-do run { target sse2 } } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

static void
sse2_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr57233.c"
