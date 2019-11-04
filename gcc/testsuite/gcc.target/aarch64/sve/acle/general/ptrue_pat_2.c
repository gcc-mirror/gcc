/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (int *last, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b16 (SV_VL16);
  *last = svptest_last (svptrue_b16 (), res);
  *ptr = res;
}

int
test2 ()
{
  svbool_t res = svptrue_pat_b16 (SV_VL16);
  return svptest_last (svptrue_b16 (), res);
}

/* { dg-final { scan-assembler-times {\tptrues\tp[0-9]+\.h, vl16\n} 2 } } */
/* { dg-final { scan-assembler-not {\tptrue\t} { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
