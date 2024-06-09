/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* This file just generates calls to the various builtins and verifies the
   expected number of instructions for each builtin were generated.  */

#include "vsx-vector-6-func-2op.h"

/* { dg-final { scan-assembler-times {\mxvaddsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvdivsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmulsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvsubsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvdivdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmindp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmaxsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvminsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmaxdp\M} 1 } } */
