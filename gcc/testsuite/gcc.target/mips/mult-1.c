/* For SI->DI widening multiplication we should use DINS to combine the two
   halves.  For Octeon use DMUL with explicit widening.  */
/* This test requires widening_mul */
/* { dg-options "-mgp64 (HAS_INS) (NOT_HAS_DMUL) -fexpensive-optimizations" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tdins\t" } } */
/* { dg-final { scan-assembler-not "\tdsll\t" } } */
/* { dg-final { scan-assembler-not "\tdsrl\t" } } */
/* { dg-final { scan-assembler-not "\tor\t" } } */

NOMIPS16 unsigned long long
f (unsigned int i, unsigned int j)
{
  i++;
  return (unsigned long long) i * j;
}
