/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-final { scan-assembler "\tdmul\t" } } */
/* { dg-final { scan-assembler-not "\tdmult\t" } } */
/* { dg-final { scan-assembler-not "\tmflo\t" } } */

NOMIPS16 long long
f (long long a, long long b)
{
  return a * b;
}
