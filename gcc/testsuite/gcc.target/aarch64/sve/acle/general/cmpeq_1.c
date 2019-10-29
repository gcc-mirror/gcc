/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svint8_t x, svint64_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpeq_wide (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svint8_t x, svint64_t y, int *any)
{
  svbool_t res = svcmpeq_wide (pg, x, y);
  return svptest_any (pg, res);
}

/* { dg-final { scan-assembler-times {\tcmpeq\t} 2 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
