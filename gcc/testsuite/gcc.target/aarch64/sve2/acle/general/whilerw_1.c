/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (int8_t *x, int8_t *y, int *any, svbool_t *ptr)
{
  svbool_t res = svwhilerw (x, y);
  *any = svptest_last (svptrue_b8 (), res);
  *ptr = res;
}

int
test2 (int8_t *x, int8_t *y)
{
  svbool_t res = svwhilerw (x, y);
  return svptest_last (svptrue_b8 (), res);
}

/* { dg-final { scan-assembler-times {\twhilerw\t} 2 } } */
/* { dg-final { scan-assembler-not {\tptrue\t} } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
