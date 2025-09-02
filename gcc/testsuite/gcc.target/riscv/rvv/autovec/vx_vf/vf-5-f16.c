/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -fno-fast-math --param=fpr2vr-cost=0" } */

#include "vf_binop.h"

DEF_VF_BINOP_CASE_2_WRAP (_Float16, __builtin_fminf16, min)

/* { dg-final { scan-assembler-times {vfmin.vf} 1 } } */
