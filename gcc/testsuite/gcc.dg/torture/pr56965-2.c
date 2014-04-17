extern void abort (void);

struct S { int i; int j; };
struct X { struct S s; int k; };
struct Y { int k; struct S s; };
union U { struct X x; struct Y y; } __attribute__((may_alias));

int __attribute__((noinline))
foo (union U *p, union U *q)
{
  p->x.s.j = 1;
  q->y.s.i = 0;
  return p->x.s.j;
}

struct R { int i; int j; } __attribute__((may_alias));

int __attribute__((noinline))
bar (struct R *p, struct R *q)
{
  p->i = 1;
  q->j = 0;
  return p->i;
}

int main()
{
  int a[3];
  if (foo ((union U *)&a[0], (union U *)&a[0]) != 0)
    abort ();
  if (bar ((struct R *)&a[1], (struct R *)&a[0]) != 0)
    abort ();
  return 0;
}
