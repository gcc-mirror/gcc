/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d --param=fpr2vr-cost=4" } */

#include "vf-3-f16.c"

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
/* { dg-final { scan-assembler-not {vfmul.vf} } } */
/* { dg-final { scan-assembler-not {vfadd.vf} } } */
/* { dg-final { scan-assembler-not {vfsub.vf} } } */
/* { dg-final { scan-assembler-not {vfrdiv.vf} } } */
/* { dg-final { scan-assembler-not {vfrsub.vf} } } */
/* { dg-final { scan-assembler-not {vfmin.vf} } } */
/* { dg-final { scan-assembler-not {vfmax.vf} } } */
/* { dg-final { scan-assembler-not {vfwmul.vf} } } */
/* { dg-final { scan-assembler-not {vfwadd.vf} } } */
/* { dg-final { scan-assembler-not {vfwsub.vf} } } */
/* { dg-final { scan-assembler-not {vfwadd.wf} } } */
/* { dg-final { scan-assembler-not {vfwsub.wf} } } */
/* { dg-final { scan-assembler {fcvt.s.h} } } */
