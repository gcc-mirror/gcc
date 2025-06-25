/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=4" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_1 (double, +, +, add, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (double, -, +, sub, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (double, +, -, nadd, VF_MULOP_BODY_X16)
DEF_VF_MULOP_CASE_1 (double, -, -, nsub, VF_MULOP_BODY_X16)

/* { dg-final { scan-assembler-not {vfmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfmsub.vf} } } */
/* { dg-final { scan-assembler-not {vfnmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfnmsub.vf} } } */
