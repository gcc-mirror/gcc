/* { dg-options "-O -mgp32 (-mips16)" } */
/* { dg-final { scan-assembler "\tmult\t" } } */
/* { dg-final { scan-assembler "\tmflo\t" } } */
/* { dg-final { scan-assembler "\tmfhi\t" } } */
/* { dg-final { scan-assembler-not "\tdsll\t" } } */
/* { dg-final { scan-assembler-not "\tdsrl\t" } } */

typedef int DI __attribute__((mode(DI)));
typedef int SI __attribute__((mode(SI)));

MIPS16 DI
f (SI x, SI y)
{
  return (DI) x * y;
}
