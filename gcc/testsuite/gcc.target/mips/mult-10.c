/* This test requires widening_mul */
/* { dg-options "-mgp64 (-mips16) -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
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
