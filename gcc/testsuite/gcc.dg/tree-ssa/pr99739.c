/* PR tree-optimization/99739 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "__builtin_abort \\\(\\\);" "optimized" } } */

static inline int
foo (int i, int j, int k)
{
  int x = 1;
  if (i && j && k)
    x = 2;
  if (i && j && k)
    return x;
  return -1;
}

void
bar (int i, int j, int k)
{
  if (foo (i, j, k) == 1)
    __builtin_abort ();
}

static inline int
baz (int i, int j, int k)
{
  int x = 1;
  if (i && j && k)
    x = 2;
  if (i && k && j)
    return x;
  return -1;
}

void
qux (int i, int j, int k)
{
  if (baz (i, j, k) == 1)
    __builtin_abort ();
}
