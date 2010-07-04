/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int p;
int r;

__attribute__ ((noinline))
static int a(void)
{
  return p;
}
int foo(int argc)
{
  int q;
  q = a();

  /* We should be able to move the call to a into the if path.
     in a perfect world, we'd actually decide that it can't touch
     r, and not recompute it at all!.  */
  if (argc)
    r = 9;
  return q + a();
}
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
