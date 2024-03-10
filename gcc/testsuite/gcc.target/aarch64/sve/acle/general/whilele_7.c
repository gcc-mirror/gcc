/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* { dg-final { scan-assembler-not {\twhilel[et]\t} } } */
/* { dg-final { scan-assembler-not {\tpfalse\t} } } */

void
test1 (svbool_t *ptr)
{
  *ptr = svwhilele_b8_s32 (-svcnth (), svcnth () - 1);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.b, all\n} } } */

void
test2 (svbool_t *ptr)
{
  *ptr = svwhilele_b16_s64 (1, svcntw () * 2);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.h, all\n} } } */

void
test3 (svbool_t *ptr)
{
  *ptr = svwhilele_b32_s32 (svcntd (), svcntw () + svcntd () - 1);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-9]+\.s, all\n} } } */
