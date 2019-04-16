/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2 -mdejagnu-cpu=power8" } */

/* Expected instruction counts for Power 8.  */

/* { dg-final { scan-assembler-times "xvabsdp" 1 } } */
/* { dg-final { scan-assembler-times "xvadddp" 1 } } */
/* { dg-final { scan-assembler-times "xxlnor" 6 { target le } } } */
/* { dg-final { scan-assembler-times "xxlnor" 5 { target be } } } */

/* We generate xxlor instructions for many reasons other than or'ing vector
   operands or calling __builtin_vec_or(), which  means we cannot rely on
   their usage counts being stable.  Therefore, we just ensure at least one
   xxlor instruction was generated.  */
/* { dg-final { scan-assembler "xxlor" } } */

/* { dg-final { scan-assembler-times {\mxvcmpeqdp\s} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcmpeqdp\.\s} 5 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtdp\s} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgtdp\.\s} 6 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgedp\s} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcmpgedp\.\s} 4 } } */
/* { dg-final { scan-assembler-times "xvrdpim" 1 } } */
/* { dg-final { scan-assembler-times "xvmaddadp" 1 } } */
/* { dg-final { scan-assembler-times "xvmsubadp" 1 } } */
/* { dg-final { scan-assembler-times "xvsubdp" 1 } } */
/* { dg-final { scan-assembler-times "xvmaxdp" 1 } } */
/* { dg-final { scan-assembler-times "xvmindp" 1 } } */
/* { dg-final { scan-assembler-times "xvmuldp" 1 } } */
/* { dg-final { scan-assembler-times "vperm" 1 } } */
/* { dg-final { scan-assembler-times "xvrdpic" 1 } } */
/* { dg-final { scan-assembler-times "xvsqrtdp" 1 } } */
/* { dg-final { scan-assembler-times "xvrdpiz" 1 } } */
/* { dg-final { scan-assembler-times "xvmsubasp" 1 } } */
/* { dg-final { scan-assembler-times "xvnmaddasp" 1 } } */
/* { dg-final { scan-assembler-times "xvnmaddadp" 1 } } */
/* { dg-final { scan-assembler-times "xvnmsubadp" 1 } } */
/* { dg-final { scan-assembler-times "vmsumshs" 1 } } */
/* { dg-final { scan-assembler-times "xxland" 13 } } */
/* { dg-final { scan-assembler-times "xxlxor" 2 } } */
/* { dg-final { scan-assembler-times "xxsel" 2 } } */
/* { dg-final { scan-assembler-times "xvrdpip" 1 } } */
/* { dg-final { scan-assembler-times "xvdivdp" 1 } } */
/* { dg-final { scan-assembler-times "xvrdpi" 5 } } */

/* Source code for the test in vsx-vector-6.h */
#include "vsx-vector-6.h"
