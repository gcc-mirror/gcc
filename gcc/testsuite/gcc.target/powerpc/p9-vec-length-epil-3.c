/* { dg-do compile { target { lp64 && powerpc_vsx_ok } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=1" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* Test for that only vectorize the epilogue with vector access with length,
   the main body still use normal vector load/store.  */

#include "p9-vec-length-3.h"

/* { dg-final { scan-assembler-not   {\mlxv\M} } } */
/* { dg-final { scan-assembler-not   {\mstxv\M} } } */
/* { dg-final { scan-assembler-not   {\mlxvx\M} } } */
/* { dg-final { scan-assembler-not   {\mstxvx\M} } } */
/* 64bit types get completely unrolled, so only check the others.  */
/* { dg-final { scan-assembler-times {\mlxvl\M} 14 } } */
/* { dg-final { scan-assembler-times {\mstxvl\M} 7 } } */

