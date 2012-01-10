/* { dg-options "-O -mgp64 (-mips16)" } */
/* { dg-final { scan-assembler "\tdmult\t" } } */
/* { dg-final { scan-assembler "\tmfhi\t" } } */
/* { dg-final { scan-assembler "\tmflo\t" } } */

typedef int TI __attribute__((mode(TI)));
typedef int DI __attribute__((mode(DI)));

MIPS16 TI
f (DI x, DI y)
{
  return (TI) x * y;
}
