/* This test requires widening_mul */
/* { dg-options "-mgp64 (-mips16) -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tdmult\t" } } */
/* { dg-final { scan-assembler "\tmfhi\t" } } */
/* { dg-final { scan-assembler-not "\tmflo\t" } } */

typedef int TI __attribute__((mode(TI)));
typedef int DI __attribute__((mode(DI)));

MIPS16 DI
f (DI x, DI y)
{
  return ((TI) x * y) >> 64;
}
