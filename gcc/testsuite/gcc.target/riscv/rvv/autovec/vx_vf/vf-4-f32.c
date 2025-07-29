/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=4" } */

#include "vf-3-f32.c"

/* { dg-final { scan-assembler-not {vfmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfmsub.vf} } } */
/* { dg-final { scan-assembler-not {vfnmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfnmsub.vf} } } */
/* { dg-final { scan-assembler-not {vfmacc.vf} } } */
/* { dg-final { scan-assembler-not {vfmsac.vf} } } */
/* { dg-final { scan-assembler-not {vfnmacc.vf} } } */
/* { dg-final { scan-assembler-not {vfnmsac.vf} } } */
/* { dg-final { scan-assembler-not {vfwmacc.vf} } } */
/* { dg-final { scan-assembler-not {vfwmsac.vf} } } */
/* { dg-final { scan-assembler-not {vfwnmacc.vf} } } */
/* { dg-final { scan-assembler-not {vfwnmsac.vf} } } */
/* { dg-final { scan-assembler {fcvt.d.s} } } */
