/* Use DMUL for widening multiplication too.  */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\tdmul\t" 2 } } */
/* { dg-final { scan-assembler-not "\td?mult\t" } } */
/* { dg-final { scan-assembler-times "\tdext\t" 2 } } */

NOMIPS16 long long
f (int i, int j)
{
  i++;
  return (long long) i * j;
}

NOMIPS16 unsigned long long
g (unsigned int i, unsigned int j)
{
  i++;
  return (unsigned long long) i * j;
}
