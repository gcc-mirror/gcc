/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

struct f
{
  int i;
  int j;
};

struct g
{
  int i;
  struct f j;
  struct f *k;
};

int foo(struct f *x, struct f *y)
{
  return &x->j == &y->j; /* x == y */
}

struct f y;
int bar(struct f *x)
{
  return &x->j == &y.j; /* x == &y */
}

struct g yy;
int foobar(struct g *x)
{
  return &x->j.i == &yy.j.i; /* x == &yy */
}
int foobar2(struct g *x)
{
  return &x->k->i == &yy.k->i; /* x->k == yy.k */
}

/* { dg-final { scan-tree-dump-times "x == y" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "x == &y" 2 "gimple" } } */
/* { dg-final { scan-tree-dump "x->k" "gimple" } } */
/* { dg-final { scan-tree-dump "yy.k" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
