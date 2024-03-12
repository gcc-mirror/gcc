/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo1(unsigned int x, unsigned int y)
{
  return __builtin_popcount (x&y) + __builtin_popcount (x|y);
}

int foo2(unsigned int x, unsigned int y)
{
  return __builtin_popcount (x&y) + __builtin_popcount (y|x);
}

int foo3(unsigned int x, unsigned int y)
{
  return __builtin_popcount (x|y) + __builtin_popcount (x&y);
}

int foo4(unsigned int x, unsigned int y)
{
  return __builtin_popcount (x|y) + __builtin_popcount (y&x);
}

/* { dg-final { scan-tree-dump-not " & " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
