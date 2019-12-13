/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
test1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b64 (SV_VL7);
  *any = svptest_any (svptrue_b64 (), res);
  *ptr = res;
}

int
test2 ()
{
  svbool_t res = svptrue_pat_b64 (SV_VL7);
  return svptest_any (svptrue_b64 (), res);
}

/* { dg-final { scan-assembler-times {\tptrues\tp[0-9]+\.d, vl7\n} 2 } } */
/* { dg-final { scan-assembler-not {\tptrue\t} { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not {\tptest\t} } } */
