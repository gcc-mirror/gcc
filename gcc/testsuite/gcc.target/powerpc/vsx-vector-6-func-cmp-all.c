/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* This file just generates calls to the various builtins and verifies the      
   expected number of instructions for each builtin were generated.  */

#include "vsx-vector-6-func-cmp-all.h"

/* { dg-final { scan-assembler-times {\mxvcmpeqsp\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtsp\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgesp\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxvcmpeqdp\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgedp\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtdp\M} 4 } } */


