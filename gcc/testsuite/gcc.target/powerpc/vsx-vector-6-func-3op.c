/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* This file just generates calls to the various builtins and verifies the
   expected number of instructions for each builtin were generated.  */

#include "vsx-vector-6-func-3op.h"

/* { dg-final { scan-assembler-times {\mvmsumshs\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmsumuhs\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmaddmsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmaddmdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmsubmsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmsubmdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\m(?:v|xx)permr?\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxsel\M} 2 } } */
