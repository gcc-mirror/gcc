/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

/* Test for fully with length, the loop body uses vector access with length,
   there should not be any epilogues.  Note for [u]int64_t and double we
   can avoid peeling for gaps and do not require with-len.  */

#include "p9-vec-length-8.h"

/* { dg-final { scan-assembler-times {\mlxvl\M} 21 } } */
/* { dg-final { scan-assembler-times {\mstxvl\M} 7 } } */
