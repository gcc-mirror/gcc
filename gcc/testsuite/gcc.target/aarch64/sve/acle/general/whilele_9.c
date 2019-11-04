/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* { dg-final { scan-assembler-not {\twhilele\t} } } */
/* { dg-final { scan-assembler-not {\twhilelt\t} } } */

void
test1 (svbool_t *ptr)
{
  *ptr = svwhilele_b32_u32 (1, 3);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.s, vl3\n} } } */

void
test2 (svbool_t *ptr)
{
  *ptr = svwhilele_b16_u64 (svcntd (), svcntd () + 5);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.h, vl6\n} } } */

void
test3 (svbool_t *ptr)
{
  *ptr = svwhilele_b8_u32 (0x7ffffffb, 0x80000002);
}

/* { dg-final { scan-assembler {\tptrue\tp[0-7]\.b, vl8\n} } } */
