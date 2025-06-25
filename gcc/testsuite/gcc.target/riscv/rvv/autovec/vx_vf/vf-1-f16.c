/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_0 (_Float16, +, +, add)
DEF_VF_MULOP_CASE_0 (_Float16, -, +, sub)
DEF_VF_MULOP_CASE_0 (_Float16, +, -, nadd)
DEF_VF_MULOP_CASE_0 (_Float16, -, -, nsub)

/* { dg-final { scan-assembler-times {vfmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsub.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfnmsub.vf} 1 } } */
