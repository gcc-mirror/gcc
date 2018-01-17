/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O0 -mno-fold-gimple" } */

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
   vec_packsu vpksdus
   vec_perm vperm
   vec_round xvrdpi
   vec_sel xxsel
   vec_xor xxlxor 
   vec_rsqrt  xvrsqrtesp
   vec_rsqrte xvrsqrtesp  */

/* { dg-final { scan-assembler-times "vcmpequd." 4 } } */
/* { dg-final { scan-assembler-times "vcmpgtud." 8 } } */
/* { dg-final { scan-assembler-times "xxland" 29 } } */
/* { dg-final { scan-assembler-times "vclzb" 2 } } */
/* { dg-final { scan-assembler-times "vclzb" 2 } } */
/* { dg-final { scan-assembler-times "vclzw" 2 } } */
/* { dg-final { scan-assembler-times "vclzh" 2 } } */
/* { dg-final { scan-assembler-times "xvcpsgnsp" 1 } } */
/* { dg-final { scan-assembler-times "xvmuldp" 6 } } */
/* { dg-final { scan-assembler-times "xvcvdpsxds" 1 } } */
/* { dg-final { scan-assembler-times "vctsxs" 1 } } */
/* { dg-final { scan-assembler-times "xvcvdpuxds" 1 } } */
/* { dg-final { scan-assembler-times "vctuxs" 1 } } */
/* { dg-final { scan-assembler-times "divd" 4 } } */
/* { dg-final { scan-assembler-times "divdu" 2 } } */
/* { dg-final { scan-assembler-times "vmrghb" 3 } } */
/* { dg-final { scan-assembler-times "vmrghh" 4 } } */
/* { dg-final { scan-assembler-times "xxmrghw" 4 } } */
/* { dg-final { scan-assembler-times "xxmrglw" 1 } } */
/* { dg-final { scan-assembler-times "vmrglh" 3 } } */
/* { dg-final { scan-assembler-times "mulld" 4 } } */
/* { dg-final { scan-assembler-times "xxlnor" 19 } } */
/* { dg-final { scan-assembler-times "xxlor" 14 } } */
/* { dg-final { scan-assembler-times "vpksdus" 1 } } */
/* { dg-final { scan-assembler-times "vperm" 2 } } */
/* { dg-final { scan-assembler-times "xvrdpi" 1 } } */
/* { dg-final { scan-assembler-times "xxsel" 6 } } */
/* { dg-final { scan-assembler-times "xxlxor" 6 } } */

/* The test code is in builtins -1.h.  */
#include "builtins-1.h"
