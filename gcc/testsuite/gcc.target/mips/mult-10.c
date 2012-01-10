/* { dg-options "-O2 -mgp64 (-mips16)" } */
/* { dg-final { scan-assembler "\tmult\t" } } */
/* { dg-final { scan-assembler-not "\tmflo\t" { xfail *-*-* } } } */
/* { dg-final { scan-assembler "\tmfhi\t" } } */

typedef int DI __attribute__((mode(DI)));
typedef int SI __attribute__((mode(SI)));

MIPS16 SI
f (SI x, SI y)
{
  return ((DI) x * y) >> 32;
}
