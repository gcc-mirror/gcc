/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-O3" } */

#include "mve-compare-scalar-1.c"

/* 64-bit vectors.  */
/* vmvn is used by 'ne' comparisons.  */
/* { dg-final { scan-assembler-times {\tvmvn\td[0-9]+, d[0-9]+\n} 6 } } */

/* { 8 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i8\td[0-9]+, d[0-9]+, d[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s8\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u8\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s8\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u8\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */

/* { 16 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i16\td[0-9]+, d[0-9]+, d[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s16\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u16\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s16\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u16\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */

/* { 32 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i32\td[0-9]+, d[0-9]+, d[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s32\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u32\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s32\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u32\td[0-9]+, d[0-9]+, d[0-9]+\n} 2 } } */

/* 128-bit vectors.  */

/* vmvn is used by 'ne' comparisons.  */
/* { dg-final { scan-assembler-times {\tvmvn\tq[0-9]+, q[0-9]+\n} 6 } } */

/* { 8 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */

/* { 16 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */

/* { 32 bits } x { eq, ne, lt, le, gt, ge }.  */
/* { dg-final { scan-assembler-times {\tvceq.i32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
