/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"
#include "vf_binop.h"

DEF_VF_MULOP_CASE_0 (double, +, +, add)
DEF_VF_MULOP_CASE_0 (double, -, +, sub)
DEF_VF_MULOP_CASE_0 (double, +, -, nadd)
DEF_VF_MULOP_CASE_0 (double, -, -, nsub)
DEF_VF_MULOP_ACC_CASE_0 (double, +, +, acc)
DEF_VF_MULOP_ACC_CASE_0 (double, -, +, sac)
DEF_VF_MULOP_ACC_CASE_0 (double, +, -, nacc)
DEF_VF_MULOP_ACC_CASE_0 (double, -, -, nsac)
DEF_VF_BINOP_CASE_0 (double, *, mul)
DEF_VF_BINOP_CASE_0 (double, +, add)
DEF_VF_BINOP_CASE_0 (double, -, sub)
DEF_VF_BINOP_REVERSE_CASE_0 (double, /, rdiv)
DEF_VF_BINOP_REVERSE_CASE_0 (double, -, rsub)
DEF_VF_BINOP_CASE_2_WRAP (double, MIN_FUNC_0_WRAP (double), min)
DEF_VF_BINOP_CASE_2_WRAP (double, MIN_FUNC_1_WRAP (double), min)
DEF_VF_BINOP_CASE_2_WRAP (double, MAX_FUNC_0_WRAP (double), max)
DEF_VF_BINOP_CASE_2_WRAP (double, MAX_FUNC_1_WRAP (double), max)

/* { dg-final { scan-assembler-times {vfmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsac.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmsac.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmul.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfrdiv.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfrsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmin.vf} 2 } } */
/* { dg-final { scan-assembler-times {vfmax.vf} 2 } } */
