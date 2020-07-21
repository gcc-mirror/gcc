/* { dg-do compile { target { lp64 && powerpc_p9vector_ok } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -ffast-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* Test for fully with length, the loop body uses vector access with length,
   there should not be any epilogues.  */

#include "p9-vec-length-7.h"

/* Each type has one stxvl excepting for int8 and uint8, that have two due to
   rtl pass bbro duplicating the block which has one stxvl.  */
/* { dg-final { scan-assembler-times {\mstxvl\M} 12 } } */
