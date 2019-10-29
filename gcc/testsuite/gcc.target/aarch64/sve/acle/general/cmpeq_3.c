/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svfloat32_t x, svfloat32_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpeq (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svfloat32_t x, svfloat32_t y, int *any)
{
  svbool_t res = svcmpeq (pg, x, y);
  return svptest_any (pg, res);
}

void
test3 (svbool_t pg, svfloat32_t x, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpeq (pg, x, 0.0);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test4 (svbool_t pg, svfloat32_t x, int *any)
{
  svbool_t res = svcmpeq (pg, x, 0.0);
  return svptest_any (pg, res);
}

/* { dg-final { scan-assembler-times {\tfcmeq\t} 4 } } */
/* { dg-final { scan-assembler-times {\tfcmeq\t[^\n]*, #0\.0} 2 } } */
/* { dg-final { scan-assembler-times {\tptest\t} 4 } } */
