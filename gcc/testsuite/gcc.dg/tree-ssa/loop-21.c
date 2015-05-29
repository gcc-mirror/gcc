/* PR tree-optimization/30322 */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern void op( int, int);
void foo(int f0, int f1, int e0, int e1)
{
  int i0, i1;

  for (i1 = f1; i1 <= e1; ++i1)
    for (i0 = f0; i0 <= e0; ++i0)
      op(i0, i1);
}

/* { dg-final { scan-tree-dump-times "~" 0 "optimized" } } */
