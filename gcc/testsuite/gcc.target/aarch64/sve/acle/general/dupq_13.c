/* { dg-options "-O2" } */

#include <arm_sve.h>

svbool_t
test1 (int x0, int x1)
{
  return svand_z (svptrue_b8 (), svdupq_b64 (x0, x1), svptrue_b16 ());
}

svbool_t
test2 (int x0, int x1, int x2, int x3)
{
  return svand_z (svptrue_b8 (), svdupq_b32 (x0, x1, x2, x3), svptrue_b16 ());
}

svbool_t
test3 (int x0, int x1, int x2, int x3)
{
  return svand_z (svptrue_b32 (), svdupq_b32 (x0, x1, x2, x3), svptrue_b16 ());
}

svbool_t
test4 (int x0, int x1, int x2, int x3)
{
  return svand_z (svptrue_b32 (), svdupq_b32 (x0, x1, x2, x3), svptrue_b32 ());
}

svbool_t
test5 (int x0, int x1, int x2, int x3)
{
  return svand_z (svptrue_b8 (),
		  svdupq_b16 (x0, x1, x2, x3, x2, x0, x1, x3),
		  svptrue_b32 ());
}

svbool_t
test6 (int x0, int x1, int x2, int x3)
{
  return svand_z (svptrue_b64 (),
		  svdupq_b16 (x0, x1, x2, x3, x2, x0, x1, x3),
		  svptrue_b16 ());
}

/* { dg-final { scan-assembler-not {\tand\tp} } } */
