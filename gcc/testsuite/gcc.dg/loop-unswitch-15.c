/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-optimized" } */

void bar();
void baz();
void foo (int a, int b, int n)
{
  for (int i = 0; i < n; ++i)
    if (a < b)
      bar ();
    else
      baz ();
}

/* { dg-final { scan-tree-dump "unswitching loop . on .if. with condition:" "unswitch" } } */
