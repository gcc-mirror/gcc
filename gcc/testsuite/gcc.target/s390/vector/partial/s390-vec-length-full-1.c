/* { dg-do compile { target { lp64 && s390_vx } } } */
/* { dg-options "-march=native -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* Test that the loop body uses vector load/store with length,
   there should not be any epilogues.  */

#include "s390-vec-length-1.h"

/* { dg-final { scan-assembler-times {\mvll\M} 20 } } */
/* { dg-final { scan-assembler-times {\mvstl\M} 10 } } */
