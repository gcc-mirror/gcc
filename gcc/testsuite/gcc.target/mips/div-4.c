/* { dg-options "-O -mgp64 (-mips16)" } */
/* { dg-final { scan-assembler "\tddivu\t" } } */
/* { dg-final { scan-assembler-not "\tmflo\t" } } */
/* { dg-final { scan-assembler "\tmfhi\t" } } */

typedef unsigned int DI __attribute__((mode(DI)));

MIPS16 DI
f (DI x, DI y)
{
  return x % y;
}
