/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre" } */

struct a
{
  union
  {
    int a;
    int b;
  };
  union
  {
    int c;
    int d;
  };
};

int f(struct a *c)
{
  int d = c->a;
  c->c = 1;
  return c->a + d;
}

/* We should have CSEd the load from c->a.  */

/* { dg-final { scan-tree-dump-times "c_.*\\\.a" 1 "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
