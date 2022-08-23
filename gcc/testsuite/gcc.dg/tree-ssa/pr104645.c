/* PR tree-optimization/104645 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " = PHI <" "optimized" } } */

int
foo (unsigned i)
{
  return i ? i % 2 : 0;
}

int
bar (unsigned i)
{
  int b = 0;
  if (i)
    {
      unsigned a = i & 1;
      b = a;
    }
  return b;
}

int
baz (unsigned i)
{
  return i ? i + 4 : 4;
}
