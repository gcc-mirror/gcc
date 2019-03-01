/* { dg-do compile { target { lp64 && be } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2 -mdejagnu-cpu=power7 -dp" } */

/* Expected instruction counts for Power 7 */

/* { dg-final { scan-assembler-times "xvabsdp" 1 } } */
/* { dg-final { scan-assembler-times "xvadddp" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor" 5 } } */
/* { dg-final { scan-assembler-times {\mxvcmpeqdp\s} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcmpeqdp\.\s} 5 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtdp\s} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtdp\.\s} 5 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgedp\s} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgedp\.\s} 6 } } */
/* { dg-final { scan-assembler-times "xvrdpim" 1 } } */
/* { dg-final { scan-assembler-times "xvmaddadp" 1 } } */
/* { dg-final { scan-assembler-times "xvmsubadp" 1 } } */
/* { dg-final { scan-assembler-times "xvsubdp" 1 } } */
/* { dg-final { scan-assembler-times "xvmaxdp" 1 } } */
/* { dg-final { scan-assembler-times "xvmindp" 1 } } */
/* { dg-final { scan-assembler-times "xvmuldp" 1 } } */
/* { dg-final { scan-assembler-times "vperm" 2 } } */
/* { dg-final { scan-assembler-times "xvrdpic" 2 } } */
/* { dg-final { scan-assembler-times "xvsqrtdp" 1 } } */
/* { dg-final { scan-assembler-times "xvrdpiz" 1 } } */
/* { dg-final { scan-assembler-times "xvmsubasp" 1 } } */
/* { dg-final { scan-assembler-times "xvnmaddasp" 1 } } */
/* { dg-final { scan-assembler-times "xvnmaddadp" 1 } } */
/* { dg-final { scan-assembler-times "xvnmsubadp" 1 } } */
/* { dg-final { scan-assembler-times "vmsumshs" 2 } } */
/* { dg-final { scan-assembler-times "xxland" 13 } } */
/* { dg-final { scan-assembler-times "xxlxor" 2 } } */
/* { dg-final { scan-assembler-times "xxsel" 4 } } */
/* { dg-final { scan-assembler-times "xvrdpip" 1 } } */
/* { dg-final { scan-assembler-times "xvdivdp" 1 } } */
/* { dg-final { scan-assembler-times "xvrdpi" 7 } } */

/* Source code for the test in vsx-vector-6.h */
#include "vsx-vector-6.h"
