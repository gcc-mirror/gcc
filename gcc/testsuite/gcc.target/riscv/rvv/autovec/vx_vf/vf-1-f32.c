/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_0 (float, +, +, add)
DEF_VF_MULOP_CASE_0 (float, -, +, sub)
DEF_VF_MULOP_CASE_0 (float, +, -, nadd)
DEF_VF_MULOP_CASE_0 (float, -, -, nsub)
DEF_VF_MULOP_ACC_CASE_0 (float, +, +, acc)
DEF_VF_MULOP_ACC_CASE_0 (float, -, +, sac)
DEF_VF_MULOP_ACC_CASE_0 (float, +, -, nacc)
DEF_VF_MULOP_ACC_CASE_0 (float, -, -, nsac)
DEF_VF_MULOP_WIDEN_CASE_0 (float, double, +, +, acc)
DEF_VF_MULOP_WIDEN_CASE_0 (float, double, -, +, sac)
DEF_VF_MULOP_WIDEN_CASE_0 (float, double, +, -, nacc)
DEF_VF_MULOP_WIDEN_CASE_0 (float, double, -, -, nsac)

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
