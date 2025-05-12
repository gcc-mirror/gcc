/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=0" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_0(double, +, add)
DEF_VF_MULOP_CASE_0(double, -, sub)

/* { dg-final { scan-assembler-times {vfmadd.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmsub.vf} 1 } } */
