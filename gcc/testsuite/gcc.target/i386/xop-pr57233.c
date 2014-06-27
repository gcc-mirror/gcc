/* PR tree-optimization/57233 */
/* { dg-do run { target xop } } */
/* { dg-options "-O2 -mxop" } */

#include "xop-check.h"

static void
xop_test (void)
{
  do_main ();
}

#undef main
#define main() do_main ()

#include "../../gcc.dg/pr57233.c"
