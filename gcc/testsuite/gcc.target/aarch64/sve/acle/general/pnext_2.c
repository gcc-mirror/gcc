/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svbool_t prev, int *last, svbool_t *ptr)
{
  svbool_t res = svpnext_b16 (pg, prev);
  *last = svptest_last (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svbool_t prev)
{
  svbool_t res = svpnext_b16 (pg, prev);
  return svptest_last (pg, res);
}

void
test3 (svbool_t pg, svbool_t prev, int *last, svbool_t *ptr)
{
  svbool_t res = svpnext_b32 (pg, prev);
  *last = svptest_last (pg, res);
  *ptr = res;
}

int
test4 (svbool_t pg, svbool_t prev)
{
  svbool_t res = svpnext_b32 (pg, prev);
  return svptest_last (pg, res);
}

void
test5 (svbool_t pg, svbool_t prev, int *last, svbool_t *ptr)
{
  svbool_t res = svpnext_b64 (pg, prev);
  *last = svptest_last (pg, res);
  *ptr = res;
}

int
test6 (svbool_t pg, svbool_t prev)
{
  svbool_t res = svpnext_b64 (pg, prev);
  return svptest_last (pg, res);
}

/* { dg-final { scan-assembler-times {\tpnext\t} 6 } } */
/* { dg-final { scan-assembler-times {\tptest\t} 6 } } */
