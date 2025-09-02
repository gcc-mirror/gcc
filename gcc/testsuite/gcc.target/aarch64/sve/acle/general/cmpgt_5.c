/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svint8_t x, svint64_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpgt_wide (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svint8_t x, svint64_t y, int *any)
{
  svbool_t res = svcmpgt_wide (pg, x, y);
  return svptest_any (pg, res);
}

void
test3 (svint8_t x, svint64_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b8 ();
  svbool_t res = svcmpgt_wide (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test4 (svint8_t x, svint64_t y, int *any)
{
  svbool_t pg = svptrue_b8 ();
  svbool_t res = svcmpgt_wide (pg, x, y);
  return svptest_any (pg, res);
}

void
test5 (svint16_t x, svint64_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpgt_wide (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test6 (svint16_t x, svint64_t y, int *any)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpgt_wide (pg, x, y);
  return svptest_any (pg, res);
}

void
test7 (svint32_t x, svint64_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpgt_wide (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test8 (svint32_t x, svint64_t y, int *any)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpgt_wide (pg, x, y);
  return svptest_any (pg, res);
}

/* { dg-final { scan-assembler-times {\tcmpgt\t} 8 } } */
/* { dg-final { scan-assembler-times {\tptrue\t} 6 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
