/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O0 -mno-fold-gimple -dp" } */
/* { dg-prune-output "gimple folding of rs6000 builtins has been disabled." } */

/* Test that a number of newly added builtin overloads are accepted
   by the compiler.  */

/* Expected results for Little Endian:
   vec_all_eq          vcmpequd.
   vec_all_ne          vcmpequd.
   vec_any_eq          vcmpequd.
   vec_any_ne          vcmpequd.
   vec_all_gt          vcmpgtud.
   vec_all_le          vcmpgtud.
   vec_any_gt          vcmpgtud.
   vec_any_lt          vcmpgtud.
   vec_any_le          vcmpgtud.
   vec_and             xxland
   vec_andc            xxlandc
   vec_cntlz           vclzd, vclzb, vclzw, vclzh
   xvcpsgnsp  vec_cpsgn
   vec_ctf    xvmuldp 
   vec_cts xvcvdpsxds, vctsxs
   vec_ctu   xvcvdpuxds, vctuxs
   vec_div   divd, divdu
   vec_mergel vmrghb, vmrghh, xxmrghw
   vec_mergeh  xxmrglw, vmrglh
   vec_mul mulld
   vec_nor xxlnor
   vec_or xxlor
   vec_packsu vpkudus
   vec_perm vperm
   vec_round xvrdpi
   vec_sel xxsel
   vec_xor xxlxor 
   vec_rsqrt  xvrsqrtesp
   vec_rsqrte xvrsqrtesp  */

/* { dg-final { scan-assembler-times {\mvcmpequd\M\.} 4 } } */
/* { dg-final { scan-assembler-times {\mvcmpgtud\M\.} 8 } } */
/* { dg-final { scan-assembler-times {\mxxland\M} 16 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 13 } } */
/* { dg-final { scan-assembler-times {\mvclzb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvclzd\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvclzw\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvclzh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvcpsgnsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvmuldp\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxvcvdpsxds\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvctsxs\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcvdpuxds\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvctuxs\M} 1 } } */
/* { dg-final { scan-assembler-times {\mdivd\M} 2 } } */
/* { dg-final { scan-assembler-times {\mdivdu\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvmrghb\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvmrghh\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmrghw\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmrglw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmrglh\M} 3 } } */
/* { dg-final { scan-assembler-times {\mmulld\M} 4 } } */
/* { dg-final { scan-assembler-times {(?n)\mxxlnor\M.*\mboolccv4si3_internal1\M} 6 } } */
/* { dg-final { scan-assembler-times {(?n)\mxxlor\M.*\mboolv4si3_internal\M} 6 } } */
/* { dg-final { scan-assembler-times {\mvpkudus\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxvrdpi\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxsel\M} 6 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 6 } } */

/* The test code is in builtins -1.h.  */
#include "builtins-1.h"
