extern void abort (void);

struct A
{
  struct A *a;
};

struct B
{
  struct A *b;
};

__attribute__((noinline))
struct A *
foo (struct A *x)
{
  asm volatile ("" : : "g" (x) : "memory");
  return x;
}

__attribute__((noinline))
void
bar (struct B *w, struct A *x, struct A *y, struct A *z)
{
  struct A **c;
  c = &w->b;
  *c = foo (x);
  while (*c)
    c = &(*c)->a;
  *c = foo (y);
  while (*c)
    c = &(*c)->a;
  *c = foo (z);
}

struct B d;
struct A e, f, g;

int
main (void)
{
  f.a = &g;
  bar (&d, &e, &f, 0);
  if (d.b == 0
      || d.b->a == 0
      || d.b->a->a == 0
      || d.b->a->a->a != 0)
    abort ();
  return 0;
}

