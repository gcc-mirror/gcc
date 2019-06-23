/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -mavx2 -fdump-tree-vect-details" } */
/* { dg-require-effective-target avx2 } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" } } */

#include "avx2-check.h"

#define main() do_main ()

#include "../../gcc.dg/vect/vect-simd-11.c"

static void
avx2_test (void)
{
  do_main ();
}
