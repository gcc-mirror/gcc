/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svbool_t prev, int *last, svbool_t *ptr)
{
  svbool_t res = svpnext_b8 (pg, prev);
  *last = svptest_last (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svbool_t prev)
{
  svbool_t res = svpnext_b8 (pg, prev);
  return svptest_last (pg, res);
}

/* { dg-final { scan-assembler-times {\tpnext\t} 2 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
