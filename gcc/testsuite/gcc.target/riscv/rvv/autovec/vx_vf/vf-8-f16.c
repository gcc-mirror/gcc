/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -fno-fast-math --param=fpr2vr-cost=4" } */

#include "vf-7-f16.c"

/* { dg-final { scan-assembler-not {vfmin.vf} } } */
/* { dg-final { scan-assembler-not {vfmax.vf} } } */
