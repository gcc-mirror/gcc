// Build don't link:

struct A {
  int a, b, c, d;
};

inline void foo (int, A &);

struct D {
};

struct E: public D {
  void f (A &y)
  {
    foo (1, y);
  }
};

struct F: public D {
  void f (A &y)
  {
    foo (2, y);
  }
};

E *d;
F *e;

inline int baz (int y)
{
  A a;
  if (y) {
    d->f (a);
  } else {
    e->f (a);
  }
  return 0;
}

inline void foo (int y, A &z)
{
  z.a = baz (y);
  z.b = baz (y);
  z.c = baz (y);
  z.d = baz (y);
}

struct G {
  E a;
  F b;
  void bar (A &);
};

void G::bar(A &y)
{
  a.f(y);
  b.f(y);
}
