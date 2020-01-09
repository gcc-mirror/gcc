/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svint16_t x, svint16_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svmatch (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svint16_t x, svint16_t y, int *any)
{
  svbool_t res = svmatch (pg, x, y);
  return svptest_any (pg, res);
}

/* These four are always false, but we don't realize that yet.  */

void
test3 (svbool_t pg, svint16_t x, svint16_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svmatch (svptrue_b8 (), x, y);
  *any = svptest_last (svptrue_b8 (), res);
  *ptr = res;
}

int
test4 (svbool_t pg, svint16_t x, svint16_t y, int *any)
{
  svbool_t res = svmatch (svptrue_b8 (), x, y);
  return svptest_last (svptrue_b8 (), res);
}

void
test5 (svbool_t pg, svint16_t x, svint16_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svmatch (svptrue_b16 (), x, y);
  *any = svptest_last (svptrue_b8 (), res);
  *ptr = res;
}

int
test6 (svbool_t pg, svint16_t x, svint16_t y, int *any)
{
  svbool_t res = svmatch (svptrue_b16 (), x, y);
  return svptest_last (svptrue_b8 (), res);
}

/* { dg-final { scan-assembler-times {\tmatch\t} 6 } } */
/* { dg-final { scan-assembler-times {\tptest\t} 6 } } */
