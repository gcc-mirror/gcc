/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-O3 -funsafe-math-optimizations" } */

#include "mve-vcmp-f32-2.c"

/* 'ne' uses vceq.  */
/* le and lt use ge and gt with inverted operands.  */
/* { dg-final { scan-assembler-times {\tvceq.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */

/* { dg-final { scan-assembler-times {\tvmov.f32\tq[0-9]+, #2.0e\+0} 6 } } */
/* { dg-final { scan-assembler-times {\tvmov.f32\tq[0-9]+, #3.0e\+0} 6 } } */
