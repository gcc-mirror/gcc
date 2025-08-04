/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svuint8_t x, svuint8_t y, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpgt (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svuint8_t x, svuint8_t y, int *any)
{
  svbool_t res = svcmpgt (pg, x, y);
  return svptest_any (pg, res);
}

void
test3 (svbool_t pg, svuint8_t x, int *any, svbool_t *ptr)
{
  svbool_t res = svcmpgt (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test4 (svbool_t pg, svuint8_t x, int *any)
{
  svbool_t res = svcmpgt (pg, x, 10);
  return svptest_any (pg, res);
}

void
test5 (svuint16_t x, svuint16_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpgt (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test6 (svuint16_t x, svuint16_t y)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpgt (pg, x, y);
  return svptest_any (pg, res);
}

void
test7 (svuint16_t x, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpgt (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test8 (svuint16_t x)
{
  svbool_t pg = svptrue_b16 ();
  svbool_t res = svcmpgt (pg, x, 10);
  return svptest_any (pg, res);
}

void
test9 (svuint32_t x, svuint32_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpgt (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test10 (svuint32_t x, svuint32_t y)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpgt (pg, x, y);
  return svptest_any (pg, res);
}

void
test11 (svuint32_t x, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpgt (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test12 (svuint32_t x)
{
  svbool_t pg = svptrue_b32 ();
  svbool_t res = svcmpgt (pg, x, 10);
  return svptest_any (pg, res);
}

void
test13 (svuint64_t x, svuint64_t y, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpgt (pg, x, y);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test14 (svuint64_t x, svuint64_t y)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpgt (pg, x, y);
  return svptest_any (pg, res);
}

void
test15 (svuint64_t x, int *any, svbool_t *ptr)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpgt (pg, x, 10);
  *any = svptest_any (pg, res);
  *ptr = res;
}

int
test16 (svuint64_t x)
{
  svbool_t pg = svptrue_b64 ();
  svbool_t res = svcmpgt (pg, x, 10);
  return svptest_any (pg, res);
}

/* { dg-final { scan-assembler-times {\tcmphi\t} 16 } } */
/* { dg-final { scan-assembler-times {\tcmphi\t[^\n]*, #10} 8 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
