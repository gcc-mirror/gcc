/* { dg-do compile } */
/* { dg-options "-O -fno-early-inlining -fdump-tree-ccp2" } */

int *p;
int inline bar(void) { return 0; }
int foo(int x)
{
  int i;
  int *q;
  if (bar())
    q = &i;
  else
    q = p;
  return *q + *p;
}

/* { dg-final { scan-tree-dump-not "NOTE: no flow-sensitive alias info for" "ccp2" } } */
/* { dg-final { cleanup-tree-dump "ccp2" } } */
