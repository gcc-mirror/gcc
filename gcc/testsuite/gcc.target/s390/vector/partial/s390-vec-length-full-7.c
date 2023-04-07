/* { dg-do compile { target { lp64 && s390_vx } } } */
/* { dg-options "-march=native -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* Test that the loop body uses vector load/store with length,
   there should not be any epilogues.  */

#include "s390-vec-length-7.h"

/* Each type should have one vstl but we do not currently vectorize the
   float and double variants and the [u]int64_t ones which do not require
   partial vectors.  */
/* { dg-final { scan-assembler-times {\mvstl\M} 6 } } */
