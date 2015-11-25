/* { dg-do compile }  */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

struct Foo
{
  int n;
  int * __restrict__ p;
};
void bar(struct Foo f, int * __restrict__ q)
{
  int i;
  for (i = 0; i < f.n; ++i)
    {
      *q += f.p[i];
    }
}

/* { dg-final { scan-tree-dump "Executing store motion" "lim2" } } */
