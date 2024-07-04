/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* This file just generates calls to the various builtins and verifies the
   expected number of instructions for each builtin were generated.  */

#include "vsx-vector-6-func-1op.h"

/* { dg-final { scan-assembler-times {\mxvabssp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrspip\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrspim\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrspi\M} 1 } } */ 
/* { dg-final { scan-assembler-times {\mxvrspic\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrspiz\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvabsdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpip\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpim\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpic\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpiz\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvsqrtdp\M} 1 } } */
