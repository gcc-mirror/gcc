/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svint8_t x, svint8_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpne (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svint8_t x, svint8_t y, int *any)
{
  svbool_t res = svcmpne (pg, x, y);
  return svptest_any (pg, res);
}

void
test3 (svbool_t pg, svint8_t x, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpne (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test4 (svbool_t pg, svint8_t x, int *any)
{
  svbool_t res = svcmpne (pg, x, 10);
  return svptest_any (pg, res);
}

void
test5 (svint16_t x, svint16_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpne (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test6 (svint16_t x, svint16_t y)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpne (pg, x, y);
  return svptest_any (pg, res);
}

void
test7 (svint16_t x, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpne (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test8 (svint16_t x)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpne (pg, x, 10);
  return svptest_any (pg, res);
}

void
test9 (svint32_t x, svint32_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpne (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test10 (svint32_t x, svint32_t y)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpne (pg, x, y);
  return svptest_any (pg, res);
}

void
test11 (svint32_t x, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpne (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test12 (svint32_t x)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpne (pg, x, 10);
  return svptest_any (pg, res);
}

void
test13 (svint64_t x, svint64_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpne (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test14 (svint64_t x, svint64_t y)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpne (pg, x, y);
  return svptest_any (pg, res);
}

void
test15 (svint64_t x, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpne (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test16 (svint64_t x)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpne (pg, x, 10);
  return svptest_any (pg, res);
}

/* { dg-final { scan-assembler-times {\tcmpne\t} 16 } } */
/* { dg-final { scan-assembler-times {\tcmpne\t[^\n]*, #10} 8 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
