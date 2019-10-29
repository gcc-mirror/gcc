/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svint8_t x, svint8_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpeq (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svint8_t x, svint8_t y, int *any)
{
  svbool_t res = svcmpeq (pg, x, y);
  return svptest_any (pg, res);
}

void
test3 (svbool_t pg, svint8_t x, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpeq (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test4 (svbool_t pg, svint8_t x, int *any)
{
  svbool_t res = svcmpeq (pg, x, 10);
  return svptest_any (pg, res);
}

/* { dg-final { scan-assembler-times {\tcmpeq\t} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\t[^\n]*, #10} 2 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
