/* { dg-do compile } */
/* { dg-options "-O -mabi=o64" } */

/* { dg-final { scan-assembler "slt\t" } } */
/* { dg-final { scan-assembler "sltu\t\|xor\t\|xori\t" } } */

/* This test should work both in mips16 and non-mips16 mode.  */

int
f (long long a, long long b)
{
  return a > 5;
}
