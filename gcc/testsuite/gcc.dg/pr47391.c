/* PR tree-optimization/47391 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

const volatile int v = 1;
int i = 0;

void
foo (void)
{
  i = v;
}

int
main (void)
{
  foo ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "i = 1;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
