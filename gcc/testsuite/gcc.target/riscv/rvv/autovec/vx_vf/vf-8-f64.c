/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=4" } */

#include "vf-7-f64.c"

/* { dg-final { scan-assembler-not {vfmin.vf} } } */
