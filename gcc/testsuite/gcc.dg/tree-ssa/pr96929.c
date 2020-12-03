/* PR tree-optimization/96929 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "baz \\\(\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times "return -1;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not " >> " "optimized" } } */

int baz (void);

int
foo (void)
{
  return -1 >> baz ();
}

int
bar (int y)
{
  int z = -1;
  return z >> y;
}
