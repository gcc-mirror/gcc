/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fno-trapping-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=1" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

/* Test for that only vectorize the epilogue with vector access with length,
   the main body still use normal vector load/store.  */

#include "p9-vec-length-6.h"

/* { dg-final { scan-assembler-times {\mlxvx?\M} 42 } } */
/* { dg-final { scan-assembler-times {\mstxvx?\M} 16 } } */
/* 64bit/32bit pairs don't have the epilogues.  */
/* { dg-final { scan-assembler-times {\mlxvl\M} 10 } } */
/* { dg-final { scan-assembler-times {\mstxvl\M} 10 } } */

