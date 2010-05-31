/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */

void foo (void)
{
  int a;

  void bar (void)
  {
    a = 1;
  }
}

/* { dg-final { scan-tree-dump-not "bar" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
