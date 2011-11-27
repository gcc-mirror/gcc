/* { dg-options "-O -mgp64 (-mips16)" } */
/* { dg-final { scan-assembler "\tdmultu\t" } } */
/* { dg-final { scan-assembler "\tmfhi\t" } } */
/* { dg-final { scan-assembler-not "\tmflo\t" } } */

typedef unsigned int TI __attribute__((mode(TI)));
typedef unsigned int DI __attribute__((mode(DI)));

MIPS16 DI
f (DI x, DI y)
{
  return ((TI) x * y) >> 64;
}
