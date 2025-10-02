/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"
#include "vf_binop.h"

DEF_VF_MULOP_CASE_1 (float, +, +, add, VF_MULOP_BODY_X128)
DEF_VF_MULOP_CASE_1 (float, -, +, sub, VF_MULOP_BODY_X128)
DEF_VF_MULOP_CASE_1 (float, +, -, nadd, VF_MULOP_BODY_X128)
DEF_VF_MULOP_CASE_1 (float, -, -, nsub, VF_MULOP_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, +, +, acc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, -, +, sac, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, +, -, nacc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, -, -, nsac, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, +, +, acc)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, -, +, sac)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, +, -, nacc)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, -, -, nsac)
DEF_VF_BINOP_CASE_1 (float, *, mul, VF_BINOP_BODY_X128)
DEF_VF_BINOP_CASE_1 (float, +, add, VF_BINOP_BODY_X128)
DEF_VF_BINOP_CASE_1 (float, -, sub, VF_BINOP_BODY_X128)
DEF_VF_BINOP_REVERSE_CASE_1 (float, /, rdiv, VF_BINOP_REVERSE_BODY_X128)
DEF_VF_BINOP_REVERSE_CASE_1 (float, -, rsub, VF_BINOP_REVERSE_BODY_X128)
DEF_VF_BINOP_CASE_3_WRAP (float, MIN_FUNC_0_WRAP (float), min,
			  VF_BINOP_FUNC_BODY_X128)
DEF_VF_BINOP_CASE_3_WRAP (float, MIN_FUNC_1_WRAP (float), min,
			  VF_BINOP_FUNC_BODY_X128)
DEF_VF_BINOP_CASE_3_WRAP (float, MAX_FUNC_0_WRAP (float), max,
			  VF_BINOP_FUNC_BODY_X128)
DEF_VF_BINOP_CASE_3_WRAP (float, MAX_FUNC_1_WRAP (float), max,
			  VF_BINOP_FUNC_BODY_X128)
DEF_VF_BINOP_WIDEN_CASE_1 (float, double, *, mul)
DEF_VF_BINOP_WIDEN_CASE_1 (float, double, +, add)
DEF_VF_BINOP_WIDEN_CASE_1 (float, double, -, sub)
DEF_VF_BINOP_WIDEN_CASE_3 (float, double, +, add)
DEF_VF_BINOP_WIDEN_CASE_3 (float, double, -, sub)

/* { dg-final { scan-assembler {vfmadd.vf} } } */
/* { dg-final { scan-assembler {vfmsub.vf} } } */
/* { dg-final { scan-assembler {vfnmadd.vf} } } */
/* { dg-final { scan-assembler {vfnmsub.vf} } } */
/* { dg-final { scan-assembler {vfmacc.vf} } } */
/* { dg-final { scan-assembler {vfmsac.vf} } } */
/* { dg-final { scan-assembler {vfnmacc.vf} } } */
/* { dg-final { scan-assembler {vfnmsac.vf} } } */
/* { dg-final { scan-assembler {vfwmacc.vf} } } */
/* { dg-final { scan-assembler {vfwmsac.vf} } } */
/* { dg-final { scan-assembler {vfwnmacc.vf} } } */
/* { dg-final { scan-assembler {vfwnmsac.vf} } } */
/* { dg-final { scan-assembler {vfmul.vf} } } */
/* { dg-final { scan-assembler {vfadd.vf} } } */
/* { dg-final { scan-assembler {vfsub.vf} } } */
/* { dg-final { scan-assembler {vfrdiv.vf} } } */
/* { dg-final { scan-assembler {vfrsub.vf} } } */
/* { dg-final { scan-assembler {vfmin.vf} } } */
/* { dg-final { scan-assembler {vfmax.vf} } } */
/* { dg-final { scan-assembler {vfwmul.vf} } } */
/* { dg-final { scan-assembler {vfwadd.vf} } } */
/* { dg-final { scan-assembler {vfwsub.vf} } } */
/* { dg-final { scan-assembler {vfwadd.wf} } } */
/* { dg-final { scan-assembler {vfwsub.wf} } } */
