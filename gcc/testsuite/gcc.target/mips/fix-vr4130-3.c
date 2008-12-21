/* { dg-do compile } */
/* { dg-options "-march=vr4130 -mgp64 -mfix-vr4130" } */
NOMIPS16 unsigned long long
foo (unsigned long long x, unsigned long long y)
{
  return x % y;
}
/* { dg-final { scan-assembler "\tdmacchi\t" } } */
