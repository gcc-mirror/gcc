/* PR tree-optimization/90693 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-abm -mno-popcnt -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "POPCOUNT \\\(" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_popcount(ll)? \\\(" "optimized" } } */

int
foo (unsigned int x)
{
  return __builtin_popcount (x) == 1;
}

int
bar (unsigned int x)
{
  return (x ^ (x - 1)) > x - 1;
}

int
baz (unsigned long long x)
{
  return __builtin_popcountll (x) == 1;
}

int
qux (unsigned long long x)
{
  return (x ^ (x - 1)) > x - 1;
}
