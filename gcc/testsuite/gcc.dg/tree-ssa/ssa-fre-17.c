/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details -fno-tree-sra" } */

struct Bar {
  int dom;
};
struct Foo {
  struct Bar doms[3];
};

int foo(int i, int j, int k)
{
  struct Foo f;

  f.doms[0].dom = i;
  f.doms[1].dom = j;
  f.doms[2].dom = k;
  return f.doms[0LL].dom;
}

/* { dg-final { scan-tree-dump "Replaced f.doms\\\[0\\\].dom with i_" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */

