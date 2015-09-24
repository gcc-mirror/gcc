// { dg-do compile }
// { dg-options "-O -fdump-tree-fre1" }

int
f (int *__restrict__ &__restrict__ p, int *p2)
{
  *p = 1;
  *p2 = 2;
  return *p;
}

// { dg-final { scan-tree-dump "return 1;" "fre1" } }
