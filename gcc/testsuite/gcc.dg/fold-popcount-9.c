/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define popc __builtin_popcount

int foo1(unsigned int x, unsigned int y)
{
  return popc (x) + popc (y) - popc (x&y);
}

int foo2(unsigned int x, unsigned int y)
{
  return popc (y) + popc (x) - popc (x&y);
}

int foo3(unsigned int x, unsigned int y)
{
  return (popc (x) - popc (x&y)) + popc (y);
}

int foo4(unsigned int x, unsigned int y)
{
  return (popc (y) - popc (x&y)) + popc (x);
}

/* { dg-final { scan-tree-dump-not " & " "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\| " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "popcount " 4 "optimized" } } */
