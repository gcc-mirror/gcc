/* { dg-do compile } */
/* Pass cunroll isn't disabled by -fno-unroll-loops, so use explicit
   disabling option for it.  */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -ffast-math" } */

/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2 -fdisable-tree-cunroll" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* Test for fully with length, the loop body uses vector access with length,
   there should not be any epilogues.  */

#include "p9-vec-length-7.h"

/* Each type has one stxvl excepting for int8 and uint8, that have two due to
   rtl pass bbro duplicating the block which has one stxvl.  */
/* { dg-final { scan-assembler-times {\mstxvl\M} 12 } } */
