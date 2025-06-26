/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_0 (double, +, +, add)
DEF_VF_MULOP_CASE_0 (double, -, +, sub)
DEF_VF_MULOP_CASE_0 (double, +, -, nadd)
DEF_VF_MULOP_CASE_0 (double, -, -, nsub)
DEF_VF_MULOP_ACC_CASE_0 (double, +, +, acc)
DEF_VF_MULOP_ACC_CASE_0 (double, -, +, sac)
DEF_VF_MULOP_ACC_CASE_0 (double, +, -, nacc)
DEF_VF_MULOP_ACC_CASE_0 (double, -, -, nsac)

/* { dg-final { scan-assembler-times {vfmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsac.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmacc.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmsac.vf} 1 } } */
