/* PR tree-optimization/13827 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

unsigned foo (unsigned a, unsigned b)
{
  return (a & 0xff00) != (b & 0xff00);
}

unsigned bar (unsigned c, unsigned d)
{
  return (c & 0xff00) == (d & 0xff00);
}

/* { dg-final { scan-tree-dump-times "a \\^ b" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "c \\^ d" 1 "original" } } */
