/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, svbool_t x, int *any, svbool_t *ptr)
{
  svbool_t res = svnot_z (pg, x);
  *any = svptest_last (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg, svbool_t x)
{
  svbool_t res = svnot_z (pg, x);
  return svptest_first (pg, res);
}

/* { dg-final { scan-assembler-times {\tnots\t} 2 } } */
/* { dg-final { scan-assembler-not {\tnot\t} } } */
