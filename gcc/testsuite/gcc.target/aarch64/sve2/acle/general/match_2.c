/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svint16_t x, svint16_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svmatch (svptrue_b8 (), x, y);
  *any = svptest_any (svptrue_b16 (), res);
  *ptr = res;
}

int
test2 (svint16_t x, svint16_t y, int *any)
{
  svbool_t res = svmatch (svptrue_b8 (), x, y);
  return svptest_any (svptrue_b16 (), res);
}

void
test3 (svint16_t x, svint16_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svmatch (svptrue_b16 (), x, y);
  *any = svptest_any (svptrue_b16 (), res);
  *ptr = res;
}

int
test4 (svint16_t x, svint16_t y, int *any)
{
  svbool_t res = svmatch (svptrue_b16 (), x, y);
  return svptest_any (svptrue_b16 (), res);
}

/* { dg-final { scan-assembler-times {\tmatch\t} 4 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
