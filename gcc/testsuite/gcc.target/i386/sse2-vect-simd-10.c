/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -msse2 -mno-sse3 -fdump-tree-vect-details" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#define main() do_main ()

#include "../../gcc.dg/vect/vect-simd-10.c"

static void
sse2_test (void)
{
  do_main ();
}
