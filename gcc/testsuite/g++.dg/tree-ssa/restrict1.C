/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim-details" } */

struct Foo
{
  Foo();
  Foo(const Foo&);
  int n;
  int * __restrict__ p;
};
void bar(Foo f, int * __restrict__ q)
{
  for (int i = 0; i < f.n; ++i)
    {
      *q += f.p[i];
    }
}

/* { dg-final { scan-tree-dump "Executing store motion" "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
