/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void foo(int);
void l(int**);
int f1(int j, int t)
{
  int p = 0;
  int *a = &p;
  l(&a);
  if (a == &p)
    return 0;
  for(int i = 0; i < j; i++)
  {
    if (a == &p) foo(p);
  }
  return 0;
}

/* We should be able to remove the call to foo because a is never equal to &p inside the loop.  */
/* { dg-final { scan-tree-dump-not "foo " "optimized"} } */
