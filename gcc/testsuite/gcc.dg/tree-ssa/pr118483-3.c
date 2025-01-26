/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/118483 */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */

/* This should optimize down to just `return 0;` */
/* as `(short)a == ~(short)a` is always false. */
int f(int a)
{
  short b = a;
  int e = ~a;
  short c = e;
  return b == c;
}
