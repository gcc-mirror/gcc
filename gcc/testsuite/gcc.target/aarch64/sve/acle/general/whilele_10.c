/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

/* { dg-final { scan-assembler-not {\twhilele\t} } } */
/* { dg-final { scan-assembler-not {\twhilelt\t} } } */
/* { dg-final { scan-assembler-not {\tptrue\t} } } */

void
test1 (svbool_t *ptr)
{
  *ptr = svwhilele_b32_u32 (-1, 0);
}

void
test2 (svbool_t *ptr)
{
  *ptr = svwhilele_b16_u64 (0x80000000, 0);
}

void
test3 (svbool_t *ptr)
{
  *ptr = svwhilele_b8_u64 (0x8000000000000001ULL, 0x7ffffffffffffffeULL);
}

/* { dg-final { scan-assembler-times {\tpfalse\tp[0-9]+\.b\n} 3 } } */
