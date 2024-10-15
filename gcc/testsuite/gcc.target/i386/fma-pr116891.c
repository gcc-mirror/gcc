/* PR middle-end/116891 */
/* { dg-do run } */
/* { dg-require-effective-target fenv } */
/* { dg-require-effective-target hard_float } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-require-effective-target fma } */
/* { dg-options "-O2 -mfma -frounding-math" } */

#include <fenv.h>
#include "fma-check.h"

#define main() do_main ()
#include "../../gcc.dg/pr116891.c"

static void
fma_test (void)
{
  do_main ();
}
