struct A
{
  int p;
};

struct B
{
  struct A n;
  struct A m;
  int x;
  int y;
  int z;
};

extern int g1, g2;

static void __attribute__((noinline)) foo (struct B *b)
{
  int t;

  t = b->n.p;
  g1 = t;
  b->n.p = t+1;
  g2 = b->m.p;

  b->m = b->n;
}

void bar (struct B *b)
{
  foo (b);
}
