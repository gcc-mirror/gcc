/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d --param=fpr2vr-cost=1" } */

#include "vf_mulop.h"

DEF_VF_MULOP_CASE_0(_Float16, +, add)
DEF_VF_MULOP_CASE_0(_Float16, -, sub)

/* { dg-final { scan-assembler-not {vfmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfmsub.vf} } } */
