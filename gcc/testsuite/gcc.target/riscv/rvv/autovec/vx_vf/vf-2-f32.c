/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=1" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_0 (float, +, +, add)
DEF_VF_MULOP_CASE_0 (float, -, +, sub)
DEF_VF_MULOP_CASE_0 (float, +, -, nadd)
DEF_VF_MULOP_CASE_0 (float, -, -, nsub)

/* { dg-final { scan-assembler-not {vfmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfmsub.vf} } } */
/* { dg-final { scan-assembler-not {vfnmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfnmsub.vf} } } */
