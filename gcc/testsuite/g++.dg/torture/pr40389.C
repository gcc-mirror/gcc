/* { dg-do run } */

template <typename V> struct S
{
  V *f, *l;
  __attribute__ ((noinline)) S (void) { f = 0, l = 0; }
  void foo (V *x)
  {
    if (x->p != 0)
      x->p->n = x->n;
    else
      f = x->n;
    if (x->n != 0)
      x->n->p = x->p;
    else
      l = x->p;
  }
  __attribute__ ((noinline)) void bar (V *x)
  {
    x->n = 0;
    x->p = l;
    if (l != 0)
      l->n = x;
    else
      f = x;
    l = x;
  }
};

struct H;

struct A
{
  S <H> k;
};

struct H
{
  A *a;
  H *p, *n;
  __attribute__ ((noinline)) H (void) { p = 0, n = 0, a = 0; }
  __attribute__ ((noinline)) H (A *b) : a (b)
  {
    p = 0;
    n = 0;
    if (a != 0)
      a->k.bar (this);
  }
  __attribute__ ((noinline)) H (const H &h) : a (h.a)
  {
    p = 0;
    n = 0;
    if (a != 0)
      a->k.bar (this);
  }
  ~H (void) { if (a != 0) a->k.foo (this); }
  H &operator= (const H &o)
  {
    if (a != 0 || &o == this)
      __builtin_abort ();
    a = o.a;
    if (a != 0)
      a->k.bar (this);
    return *this;
  }
};

__attribute__ ((noinline))
H baz (void)
{
  return H (new A);
}

H g;

int
main (void)
{
  g = baz ();
  if (g.a->k.f != &g)
    __builtin_abort ();
  return 0;
}

