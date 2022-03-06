/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-O3" } */

#include "mve-compare-1.c"

/* 64-bit vectors.  */
/* vmvn is used by 'ne' comparisons: 3 sizes * 2 (signed/unsigned) * 2
   (register/zero) = 12.  */
/* { dg-final { scan-assembler-times {\tvmvn\td[0-9]+, d[0-9]+\n} 12 } } */

/* { 8 bits } x { eq, ne, lt, le, gt, ge }. */
/* ne uses eq, lt/le only apply to comparison with zero, they use gt/ge
   otherwise.  */
/* { dg-final { scan-assembler-times {\tvceq.i8\td[0-9]+, d[0-9]+, d[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i8\td[0-9]+, d[0-9]+, #0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvclt.s8\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s8\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s8\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s8\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcge.s8\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s8\td[0-9]+, d[0-9]+, #0\n} 1 } } */

/* { 16 bits } x { eq, ne, lt, le, gt, ge }. */
/* { dg-final { scan-assembler-times {\tvceq.i16\td[0-9]+, d[0-9]+, d[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i16\td[0-9]+, d[0-9]+, #0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvclt.s16\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s16\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s16\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s16\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcge.s16\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s16\td[0-9]+, d[0-9]+, #0\n} 1 } } */

/* { 32 bits } x { eq, ne, lt, le, gt, ge }. */
/* { dg-final { scan-assembler-times {\tvceq.i32\td[0-9]+, d[0-9]+, d[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i32\td[0-9]+, d[0-9]+, #0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvclt.s32\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s32\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s32\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s32\td[0-9]+, d[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcge.s32\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s32\td[0-9]+, d[0-9]+, #0\n} 1 } } */

/* 128-bit vectors.  */

/* vmvn is used by 'ne' comparisons.  */
/* { dg-final { scan-assembler-times {\tvmvn\tq[0-9]+, q[0-9]+\n} 12 } } */

/* { 8 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i8\tq[0-9]+, q[0-9]+, #0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvclt.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcge.s8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { 16 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i16\tq[0-9]+, q[0-9]+, #0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvclt.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcge.s16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { 32 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i32\tq[0-9]+, q[0-9]+, #0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvclt.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcge.s32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
