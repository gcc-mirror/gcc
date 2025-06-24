/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_1 (float, +, +, add, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (float, -, +, sub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (float, +, -, nadd, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (float, -, -, nsub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_ACC_CASE_1 (float, +, +, acc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (float, -, +, sac, VF_MULOP_ACC_BODY_X128)

/* { dg-final { scan-assembler {vfmadd.vf} } } */
/* { dg-final { scan-assembler {vfmsub.vf} } } */
/* { dg-final { scan-assembler {vfnmadd.vf} } } */
/* { dg-final { scan-assembler {vfnmsub.vf} } } */
/* { dg-final { scan-assembler {vfmacc.vf} } } */
/* { dg-final { scan-assembler {vfmsac.vf} } } */
