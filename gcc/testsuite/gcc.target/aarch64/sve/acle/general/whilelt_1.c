/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* { dg-final { scan-assembler-not {\twhilele\t} } } */
/* { dg-final { scan-assembler-not {\twhilelt\t} } } */

void
test1 (svbool_t *ptr)
{
  *ptr = svwhilelt_b32_s32 (-8, -7);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.[bhsd], vl1\n} } } */

void
test2 (svbool_t *ptr)
{
  *ptr = svwhilelt_b16_s64 (-1, 2);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.h, vl3\n} } } */

void
test3 (svbool_t *ptr)
{
  *ptr = svwhilelt_b16_s32 (0x7ffffffa, 0x7fffffff);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.h, vl5\n} } } */

void
test4 (svbool_t *ptr)
{
  *ptr = svwhilelt_b8_s64 (svcntb (), svcntb () + 7);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.b, vl7\n} } } */

void
test5 (svbool_t *ptr)
{
  *ptr = svwhilelt_b64_s64 (0, 2);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.d, vl2\n} } } */
