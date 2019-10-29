/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <arm_sve.h>

void
foo (svint8_t *res1, svint8_t *res2, svbool_t pg, svint8_t a, svint8_t b)
{
  *res1 = svadd_m (pg, a, b);
  *res2 = svadd_m (pg, a, b);
}

/* { dg-final { scan-tree-dump-times {svadd_s8_m|svadd_m} 1 "optimized" } } */
