/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_1 (float, +, +, add, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (float, -, +, sub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (float, +, -, nadd, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (float, -, -, nsub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_ACC_CASE_1 (float, +, +, acc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, -, +, sac, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, +, -, nacc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, -, -, nsac, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, +, +, acc)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, -, +, sac)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, +, -, nacc)
DEF_VF_MULOP_WIDEN_CASE_1 (float, double, -, -, nsac)

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
