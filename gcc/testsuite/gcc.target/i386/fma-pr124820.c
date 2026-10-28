/* PR middle-end/124820 */
/* { dg-do run } */
/* { dg-require-effective-target fma } */
/* { dg-options "-O2 -mfma" } */

#include "fma-check.h"

#define main() do_main ()
#include "../../gcc.dg/pr124820.c"

static void
fma_test (void)
{
  do_main ();
}
