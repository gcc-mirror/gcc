/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-vect-raw" } */

void foo(int *p, int *q, int n)
{
  for (int i = 0; i < n; i++)
    p[i] = q[i] + 1;
}

/* { dg-final { scan-tree-dump-not {<pointer_diff_expr,} "vect" } } */
