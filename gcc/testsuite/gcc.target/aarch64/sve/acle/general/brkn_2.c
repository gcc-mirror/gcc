/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svbool_t x, svbool_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svbrkn_z (pg, x, y);
  *any = svptest_any (svptrue_b8 (), res);
  *ptr = res;
}

int
test2 (svbool_t pg, svbool_t x, svbool_t y, int *any)
{
  svbool_t res = svbrkn_z (pg, x, y);
  return svptest_any (svptrue_b8 (), res);
}

/* { dg-final { scan-assembler-times {\tbrkns\t} 2 } } */
/* { dg-final { scan-assembler-not {\tbrkn\t} } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
