/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* { dg-final { scan-assembler-not {\twhilele\t} } } */
/* { dg-final { scan-assembler-not {\twhilelt\t} } } */
/* { dg-final { scan-assembler-not {\tptrue\t} } } */

void
test1 (svbool_t *ptr)
{
  *ptr = svwhilelt_b32_s32 (0, 0);
}

void
test2 (svbool_t *ptr)
{
  *ptr = svwhilelt_b16_s64 (50, -1);
}

void
test3 (svbool_t *ptr)
{
  *ptr = svwhilelt_b16_s32 (0x7ffffffb, 0x80000000);
}

void
test4 (svbool_t *ptr)
{
  *ptr = svwhilelt_b8_s64 (svcntb (), svcntb ());
}

void
test5 (svbool_t *ptr)
{
  *ptr = svwhilelt_b8_s64 (svcntb (), svcntw ());
}

/* { dg-final { scan-assembler-times {\tpfalse\tp[0-7]\.b\n} 5 } } */
