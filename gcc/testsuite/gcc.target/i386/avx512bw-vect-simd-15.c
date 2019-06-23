/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -mavx512bw -mprefer-vector-width=512 -fdump-tree-vect-details" } */
/* { dg-require-effective-target avx512bw } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" } } */

#include "avx512bw-check.h"

#define main() do_main ()

#include "../../gcc.dg/vect/vect-simd-15.c"

static void
avx512bw_test (void)
{
  do_main ();
}
