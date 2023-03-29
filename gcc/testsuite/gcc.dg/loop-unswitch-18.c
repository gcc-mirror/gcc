/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-unswitch-optimized" } */

void bar();
void foo (int x, int n, int m)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < m; ++j)
      if (x)
        bar ();
}

/* { dg-final { scan-tree-dump "unswitching outer loop" "unswitch" } } */
