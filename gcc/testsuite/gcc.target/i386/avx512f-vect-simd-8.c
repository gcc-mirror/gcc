/* { dg-do run } */
/* { dg-options "-O2 -fopenmp-simd -mavx512f -mprefer-vector-width=512 -fdump-tree-vect-details" } */
/* { dg-require-effective-target avx512f } */
/* { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" } } */

#include "avx512f-check.h"

#define main() do_main ()

#include "../../gcc.dg/vect/vect-simd-8.c"

static void
avx512f_test (void)
{
  do_main ();
}
