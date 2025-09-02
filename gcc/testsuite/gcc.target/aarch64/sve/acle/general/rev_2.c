/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svbool_t test1()
{
  return svrev_b16 (svptrue_b16 ());
}

svbool_t test2()
{
  return svrev_b32 (svptrue_b32 ());
}

svbool_t test3()
{
  return svrev_b64 (svptrue_b64 ());
}

/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.h} } } */
/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.s} } } */
/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.d} } } */
/* { dg-final { scan-assembler-not {\tptrue\tp[0-7]\.b} } } */
/* { dg-final { scan-assembler {\trev\tp0\.h} } } */
/* { dg-final { scan-assembler {\trev\tp0\.s} } } */
/* { dg-final { scan-assembler {\trev\tp0\.d} } } */
