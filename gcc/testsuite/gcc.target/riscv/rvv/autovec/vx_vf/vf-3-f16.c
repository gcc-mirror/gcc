/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_1 (_Float16, +, +, add, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (_Float16, -, +, sub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (_Float16, +, -, nadd, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (_Float16, -, -, nsub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_ACC_CASE_1 (_Float16, +, +, acc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (_Float16, -, +, sac, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (_Float16, +, -, nacc, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_ACC_CASE_1 (_Float16, -, -, nsac, VF_MULOP_ACC_BODY_X128)
DEF_VF_MULOP_WIDEN_CASE_1 (_Float16, float, +, +, acc)
DEF_VF_MULOP_WIDEN_CASE_1 (_Float16, float, -, +, sac)

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
