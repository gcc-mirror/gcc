/* { dg-do run { target aarch64_sve_hw } } */

#include <arm_sve.h>

svfloat32_t
foo (float *ptr)
{
  svbool_t pg = svptrue_pat_b32 (SV_VL1);
  svfloat32_t res = svld1 (pg, ptr);
  return res;
}

int
main (void)
{
  svbool_t pg = svptrue_pat_b32 (SV_VL1);
  float x[1] = { 1 };
  if (svptest_any (pg, svcmpne (pg, foo (x), 1.0)))
    __builtin_abort ();
  return 0;
}
