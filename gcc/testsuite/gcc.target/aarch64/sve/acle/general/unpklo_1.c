/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svbool_t
test1 (svbool_t p)
{
  return svand_z (svptrue_b8 (), svunpklo (p), svptrue_b16 ());
}

svbool_t
test2 (svbool_t p)
{
  return svand_z (svptrue_b16 (), svunpklo (p), svptrue_b8 ());
}

svbool_t
test3 (svbool_t p)
{
  return svand_z (svptrue_b16 (), svunpklo (p), svptrue_b16 ());
}

/* { dg-final { scan-assembler-not {\tand\t} } } */
