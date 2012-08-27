/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-final { scan-assembler "\tpop\t" } } */
/* { dg-final { scan-assembler "\tdpop\t" } } */

NOMIPS16 int
f (long long a)
{
  return __builtin_popcountll (a);
}

NOMIPS16 int
g (int a)
{
  return __builtin_popcount (a);
}
