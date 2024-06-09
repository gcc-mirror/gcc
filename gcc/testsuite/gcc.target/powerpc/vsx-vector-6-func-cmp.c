/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */


/* This file just generates calls to the various builtins and verifies the
   expected number of instructions for each builtin were generated.  */

#include "vsx-vector-6-func-cmp.h"

/* { dg-final { scan-assembler-times {\mxvcmpeqsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtsp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgesp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcmpeqdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgedp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtdp\M} 2 } } */
