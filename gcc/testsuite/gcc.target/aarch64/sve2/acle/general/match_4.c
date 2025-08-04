/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svbool_t
test1 (svbool_t pg, svint16_t x, svint16_t y)
{
  return svand_z (svptrue_b8 (),
		  svmatch (pg, x, y),
		  svptrue_b16 ());
}

svbool_t
test2 (svbool_t pg, svint16_t x, svint16_t y)
{
  return svand_z (svptrue_b16 (),
		  svmatch (pg, x, y),
		  svptrue_b8 ());
}

svbool_t
test3 (svbool_t pg, svint16_t x, svint16_t y)
{
  return svand_z (svptrue_b16 (),
		  svmatch (pg, x, y),
		  svptrue_b16 ());
}

/* { dg-final { scan-assembler-not {\tand\t} } } */
