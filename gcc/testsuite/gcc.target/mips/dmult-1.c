/* { dg-do compile { target mips16_attribute } } */
/* { dg-mips-options "-mips64 -mgp64" } */
/* { dg-add-options mips16_attribute } */
/* { dg-final { scan-assembler "\tdmult\t" } } */
/* { dg-final { scan-assembler "\tmflo\t" } } */
/* { dg-final { scan-assembler-not "\tdmul\t" } } */

long long
f (long long a, long long b)
{
  return a * b;
}
