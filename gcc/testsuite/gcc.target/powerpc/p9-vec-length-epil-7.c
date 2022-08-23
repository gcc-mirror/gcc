/* { dg-do compile { target { lp64 && powerpc_p9vector_ok } } } */
/* Pass cunroll isn't disabled by -fno-unroll-loops, so use explicit
   disabling option for it.  */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -ffast-math -fdisable-tree-cunroll" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=1" } */

/* Test for that only vectorize the epilogue with vector access with length,
   the main body still use normal vector load/store.  */

#include "p9-vec-length-7.h"

/* { dg-final { scan-assembler-times {\mstxvl\M} 7 } } */
