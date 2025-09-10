/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"
#include "vf_binop.h"

DEF_VF_MULOP_CASE_0 (_Float16, +, +, add)
DEF_VF_MULOP_CASE_0 (_Float16, -, +, sub)
DEF_VF_MULOP_CASE_0 (_Float16, +, -, nadd)
DEF_VF_MULOP_CASE_0 (_Float16, -, -, nsub)
DEF_VF_MULOP_ACC_CASE_0 (_Float16, +, +, acc)
DEF_VF_MULOP_ACC_CASE_0 (_Float16, -, +, sac)
DEF_VF_MULOP_ACC_CASE_0 (_Float16, +, -, nacc)
DEF_VF_MULOP_ACC_CASE_0 (_Float16, -, -, nsac)
DEF_VF_MULOP_WIDEN_CASE_0 (_Float16, float, +, +, acc)
DEF_VF_MULOP_WIDEN_CASE_0 (_Float16, float, -, +, sac)
DEF_VF_MULOP_WIDEN_CASE_0 (_Float16, float, +, -, nacc)
DEF_VF_MULOP_WIDEN_CASE_0 (_Float16, float, -, -, nsac)
DEF_VF_BINOP_CASE_0 (_Float16, *, mul)
DEF_VF_BINOP_CASE_0 (_Float16, +, add)
DEF_VF_BINOP_CASE_0 (_Float16, -, sub)
DEF_VF_BINOP_REVERSE_CASE_0 (_Float16, /, rdiv)
DEF_VF_BINOP_REVERSE_CASE_0 (_Float16, -, rsub)
DEF_VF_BINOP_CASE_2_WRAP (_Float16, MIN_FUNC_0_WRAP (_Float16), min)
DEF_VF_BINOP_CASE_2_WRAP (_Float16, MIN_FUNC_1_WRAP (_Float16), min)
DEF_VF_BINOP_CASE_2_WRAP (_Float16, MAX_FUNC_0_WRAP (_Float16), max)
DEF_VF_BINOP_CASE_2_WRAP (_Float16, MAX_FUNC_1_WRAP (_Float16), max)
DEF_VF_BINOP_WIDEN_CASE_0 (_Float16, float, *, mul)
DEF_VF_BINOP_WIDEN_CASE_0 (_Float16, float, +, add)
DEF_VF_BINOP_WIDEN_CASE_0 (_Float16, float, -, sub)
DEF_VF_BINOP_WIDEN_CASE_2 (_Float16, float, +, add)
DEF_VF_BINOP_WIDEN_CASE_2 (_Float16, float, -, sub)

/* { dg-final { scan-assembler-times {vfmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsac.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmsac.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfwmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfwmsac.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfwnmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfwnmsac.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmul.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfrdiv.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfrsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmin.vf} 2 } } */
/* { dg-final { scan-assembler-times {vfmax.vf} 2 } } */
/* { dg-final { scan-assembler-times {vfwmul.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfwsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd.wf} 1 } } */
/* { dg-final { scan-assembler-times {vfwsub.wf} 1 } } */
