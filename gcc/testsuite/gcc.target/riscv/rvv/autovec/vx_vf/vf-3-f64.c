/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_1 (double, +, +, add, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (double, -, +, sub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (double, +, -, nadd, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (double, -, -, nsub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_ACC_CASE_1 (double, +, +, acc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (double, -, +, sac, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (double, +, -, nacc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (double, -, -, nsac, VF_MULOP_ACC_BODY_X128)

/* { dg-final { scan-assembler {vfmadd.vf} } } */
/* { dg-final { scan-assembler {vfmsub.vf} } } */
/* { dg-final { scan-assembler {vfnmadd.vf} } } */
/* { dg-final { scan-assembler {vfnmsub.vf} } } */
/* { dg-final { scan-assembler {vfmacc.vf} } } */
/* { dg-final { scan-assembler {vfmsac.vf} } } */
/* { dg-final { scan-assembler {vfnmacc.vf} } } */
/* { dg-final { scan-assembler {vfnmsac.vf} } } */
