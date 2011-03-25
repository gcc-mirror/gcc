/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

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
  int e;
};

int f(struct a *c)
{
  int d;
  c->e = 2;
  d = c->a;
  c->c = 1;
  return c->a + d;
}

/* We should have CSEd the load from c->a.  */

/* { dg-final { scan-tree-dump-times "c_.*\\\.a" 1 "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
