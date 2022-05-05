/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-O3" } */

#include "mve-vcmp-f32.c"

/* Should not be vectorized, since we do not use -funsafe-math-optimizations.  */

/* { dg-final { scan-assembler-not {\tvceq.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} } } */
/* { dg-final { scan-assembler-not {\tvcge.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} } } */
/* { dg-final { scan-assembler-not {\tvcgt.f32\tq[0-9]+, q[0-9]+, q[0-9]+\n} } } */
