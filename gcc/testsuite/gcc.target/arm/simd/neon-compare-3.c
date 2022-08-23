/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-additional-options "-O3 -funsafe-math-optimizations" } */

#include "mve-compare-3.c"


/* eq, ne, lt, le, gt, ge.  */
/* ne uses eq+vmvn, lt/le use gt/ge with swapped operands.  */
/* { dg-final { scan-assembler-times {\tvceq.f16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvmvn\tq[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcgt.f16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.f16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
