/* { dg-do compile { target { lp64 && powerpc_vsx_ok } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* Test for fully with length, the loop body uses vector access with length,
   there should not be any epilogues.  */

#include "p9-vec-length-5.h"

/* It can use normal vector load for constant vector load.  */
/* { dg-final { scan-assembler-not   {\mstxv\M} } } */
/* { dg-final { scan-assembler-not   {\mlxvx\M} } } */
/* { dg-final { scan-assembler-not   {\mstxvx\M} } } */
/* { dg-final { scan-assembler-times {\mlxvl\M} 21 } } */
/* { dg-final { scan-assembler-times {\mstxvl\M} 21 } } */
