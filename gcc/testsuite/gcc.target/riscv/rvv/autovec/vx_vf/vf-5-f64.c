/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -fno-fast-math --param=fpr2vr-cost=0" } */

#include "vf_binop.h"

DEF_VF_BINOP_CASE_2_WRAP (double, __builtin_fmin, min)
DEF_VF_BINOP_CASE_2_WRAP (double, __builtin_fmax, max)

/* { dg-final { scan-assembler-times {vfmin.vf} 1 } } */
/* { dg-final { scan-assembler-times {vfmax.vf} 1 } } */
