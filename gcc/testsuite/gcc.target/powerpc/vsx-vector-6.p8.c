/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

/* Source code for the test in vsx-vector-6.h */
#include "vsx-vector-6.h"

/* { dg-final { scan-assembler-times {\mvmaxub\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmsumshs\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmsumuhs\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvabsdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvadddp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcmpeqdp\M} 9 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgedp\M} 10 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtdp\M} 10 } } */
/* { dg-final { scan-assembler-times {\mxvdivdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmadd[am]dp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmadd[am]sp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmaxdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmindp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmsub[am]dp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmsub[am]sp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd[am]dp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvnmadd[am]sp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub[am]dp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvnmsub[am]sp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpic\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpim\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpip\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrdpiz\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvrspiz\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvsqrtdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvsubdp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxland\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 13 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 5 } } */
/* { dg-final { scan-assembler-times {\mxxlor\M} 9 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxsel\M} 2 } } */
