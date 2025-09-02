/* { dg-options "-O2" } */

#include <arm_sve.h>

svbool_t
test1 (int x)
{
  return svand_z (svptrue_b16 (), svdup_b16 (x), svptrue_b16 ());
}

svbool_t
test2 (int x)
{
  return svand_z (svptrue_b8 (), svdup_b32 (x), svptrue_b16 ());
}

svbool_t
test3 (int x)
{
  return svand_z (svptrue_b32 (), svdup_b32 (x), svptrue_b16 ());
}

svbool_t
test4 (int x)
{
  return svand_z (svptrue_b32 (), svdup_b32 (x), svptrue_b32 ());
}

svbool_t
test5 (int x)
{
  return svand_z (svptrue_b8 (), svdup_b64 (x), svptrue_b32 ());
}

svbool_t
test6 (int x)
{
  return svand_z (svptrue_b16 (), svdup_b64 (x), svptrue_b8 ());
}

svbool_t
test7 (int x)
{
  return svand_z (svptrue_b16 (), svdup_b64 (x), svptrue_b64 ());
}

/* { dg-final { scan-assembler-not {\tand\t} } } */
