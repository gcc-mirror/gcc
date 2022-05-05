/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */
/* { dg-additional-options "-O3" } */

#include "mve-vcmp.c"

/* vceq is also used for 'ne' comparisons.  */
/* { dg-final { scan-assembler-times {\tvceq.i[0-9]+\td[0-9]+, d[0-9]+, d[0-9]+\n} 12 } } */
/* { dg-final { scan-assembler-times {\tvceq.i[0-9]+\tq[0-9]+, q[0-9]+, q[0-9]+\n} 12 } } */

/* lt and le are replaced with the opposite condition, hence the double number
   of matches for gt and ge.  */
/* { dg-final { scan-assembler-times {\tvcge.s[0-9]+\td[0-9]+, d[0-9]+, d[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcge.s[0-9]+\tq[0-9]+, q[0-9]+, q[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcge.u[0-9]+\td[0-9]+, d[0-9]+, d[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcge.u[0-9]+\tq[0-9]+, q[0-9]+, q[0-9]+\n} 6 } } */

/* { dg-final { scan-assembler-times {\tvcgt.s[0-9]+\td[0-9]+, d[0-9]+, d[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s[0-9]+\tq[0-9]+, q[0-9]+, q[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u[0-9]+\td[0-9]+, d[0-9]+, d[0-9]+\n} 6 } } */
/* { dg-final { scan-assembler-times {\tvcgt.u[0-9]+\tq[0-9]+, q[0-9]+, q[0-9]+\n} 6 } } */
