/* { dg-do compile { target { ! aarch64_asm_sve2p1_ok } } } */
/* { dg-do assemble { target { aarch64_asm_sve2p1_ok } } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

/* { dg-final { scan-assembler-not {\twhilele\t} } } */
/* { dg-final { scan-assembler-not {\twhilelt\t} } } */
/* { dg-final { scan-assembler-not {\tptrue\t} } } */

void
test1 (svcount_t *ptr)
{
  *ptr = svwhilele_c8_s64 (0, -57, 2);
}

void
test2 (svcount_t *ptr)
{
  *ptr = svwhilele_c8_u64 (50, 0, 2);
}

void
test3 (svcount_t *ptr)
{
  *ptr = svwhilele_c16_s64 (7, 5, 2);
}

void
test4 (svcount_t *ptr)
{
  *ptr = svwhilele_c16_u64 (900, 100, 2);
}

void
test5 (svcount_t *ptr)
{
  *ptr = svwhilele_c32_s64 (-10, -50, 4);
}

void
test6 (svcount_t *ptr)
{
  *ptr = svwhilele_c32_u64 (1, 0, 4);
}

void
test7 (svcount_t *ptr)
{
  *ptr = svwhilele_c64_s64 (0, -5, 4);
}

void
test8 (svcount_t *ptr)
{
  *ptr = svwhilele_c64_u64 (8, 0, 4);
}

/* { dg-final { scan-assembler-times {\tpfalse\tp[0-9]+\.b\n} 8 } } */
