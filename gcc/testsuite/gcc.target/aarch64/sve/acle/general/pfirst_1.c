/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (svbool_t pg, int *last, svbool_t *ptr)
{
  svbool_t res = svpfirst (pg, svpfalse ());
  *last = svptest_last (pg, res);
  *ptr = res;
}

int
test2 (svbool_t pg)
{
  svbool_t res = svpfirst (pg, svpfalse ());
  return svptest_last (pg, res);
}

/* { dg-final { scan-assembler-times {\tpfirst\t} 2 } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
