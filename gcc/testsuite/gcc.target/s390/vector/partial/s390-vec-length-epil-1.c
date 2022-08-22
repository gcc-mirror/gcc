/* { dg-do compile { target { lp64 && s390_vx } } } */
/* { dg-options "-march=native -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=1 --param=min-vect-loop-bound=0" } */

/* Test that we only vectorize the epilogue with vector load/store with length,
   the main body still uses normal vector load/store.  */

#include "s390-vec-length-1.h"

/* { dg-final { scan-assembler-times {\mvll\M} 14 } } */
/* { dg-final { scan-assembler-times {\mvstl\M} 7 } } */

