/* { dg-options "-O2 -mgp64 (-mips16)" } */
/* { dg-final { scan-assembler "\tmult\t" } } */
/* { dg-final { scan-assembler "\tmflo\t" } } */
/* { dg-final { scan-assembler "\tmfhi\t" } } */
/* { dg-final { scan-assembler-times "\tdsll\t" 2 } } */
/* { dg-final { scan-assembler "\tdsrl\t" } } */

typedef int DI __attribute__((mode(DI)));
typedef int SI __attribute__((mode(SI)));

MIPS16 DI
f (SI x, SI y)
{
  return (DI) x * y;
}
