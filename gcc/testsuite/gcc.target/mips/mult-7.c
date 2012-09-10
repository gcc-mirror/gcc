/* { dg-options "-mgp64 (-mips16)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tdmultu?\t" } } */
/* { dg-final { scan-assembler "\tmflo\t" } } */
/* { dg-final { scan-assembler-not "\tmfhi\t" } } */

typedef unsigned int DI __attribute__((mode(DI)));

MIPS16 DI
f (DI x, DI y)
{
  return x * y;
}
