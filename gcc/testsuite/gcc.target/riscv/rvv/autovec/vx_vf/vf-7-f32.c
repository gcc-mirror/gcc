/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -fno-fast-math --param=fpr2vr-cost=0" } */

#include "vf_binop.h"

DEF_VF_BINOP_CASE_3_WRAP (float, __builtin_fminf, min, VF_BINOP_FUNC_BODY_X128)
DEF_VF_BINOP_CASE_3_WRAP (float, __builtin_fmaxf, max, VF_BINOP_FUNC_BODY_X128)

/* { dg-final { scan-assembler {vfmin.vf} } } */
/* { dg-final { scan-assembler {vfmax.vf} } } */
