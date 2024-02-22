/* { dg-do compile { target { lp64 && powerpc_vsx_ok } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* Test for fully with length, the loop body uses vector access with length,
   there should not be any epilogues.  */

#include "p9-vec-length-6.h"

/* It can use normal vector load for constant vector load.  */
/* { dg-final { scan-assembler-times {\mstxvx?\M} 6 } } */
/* 64bit/32bit pairs won't use partial vectors.  */
/* { dg-final { scan-assembler-times {\mlxvl\M} 10 } } */
/* { dg-final { scan-assembler-times {\mstxvl\M} 10 } } */
