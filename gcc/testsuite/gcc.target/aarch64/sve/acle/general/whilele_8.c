/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* { dg-final { scan-assembler-not {\tptrue\t} } } */
/* { dg-final { scan-assembler-not {\tpfalse\t} } } */

void
test1 (svbool_t *ptr)
{
  *ptr = svwhilele_b32_s32 (-4, 0);
}

void
test2 (svbool_t *ptr)
{
  *ptr = svwhilele_b16_s64 (svcntb (), svcntb () + 8);
}

void
test3 (svbool_t *ptr)
{
  *ptr = svwhilele_b64_s32 (0, 2);
}

void
test4 (svbool_t *ptr)
{
  *ptr = svwhilele_b8_s64 (16, svcntb ());
}

/* { dg-final { scan-assembler-times {\twhilel[et]\t} 4 } } */
