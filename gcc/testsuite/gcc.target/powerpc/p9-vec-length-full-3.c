/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

/* Test for fully with length, the loop body uses vector access with length,
   there should not be any epilogues.  */

#include "p9-vec-length-3.h"

/* { dg-final { scan-assembler-not   {\mlxv\M} } } */
/* { dg-final { scan-assembler-not   {\mstxv\M} } } */
/* { dg-final { scan-assembler-not   {\mlxvx\M} } } */
/* { dg-final { scan-assembler-not   {\mstxvx\M} } } */
/* 64bit types get completely unrolled, so only check the others.  */
/* { dg-final { scan-assembler-times {\mlxvl\M} 14 } } */
/* { dg-final { scan-assembler-times {\mstxvl\M} 7 } } */
