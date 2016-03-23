/* PR tree-optimization/70354 */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -mavx2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

#define main() do_main ()

#include "../../gcc.dg/vect/pr70354-1.c"

static void
avx2_test (void)
{
  do_main ();
}
