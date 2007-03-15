// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target ilp32 }
// { dg-require-effective-target fpic }
// { dg-options "-mtune=i686 -O2 -fpic" }
// { dg-bogus "\[Uu\]nresolved symbol .(_GLOBAL_OFFSET_TABLE_|\[_.A-Za-z\]\[_.0-9A-Za-z\]*@(PLT|GOT|GOTOFF))|\[Bb\]ad fixup at .DATA.:" "PIC unsupported" { xfail *-*-netware* } 0 }
class G {};

struct N {
  N *a;
};

struct V {
  typedef N *W;
  W *m, *n;
  int s() const { return int(n - m); }
  const W &operator[](int x) const { return *(m + x); }
};

struct H;

struct J {
  N *c;
  H *d;
  J(N *x, H *y) : c(x), d(y) {}
};

struct K {
  const N *c;
  const H *d;
  K(const N *x, const H *y) : c(x), d(y) {}
  K(const J &x) : c(x.c), d(x.d) {}
};

struct H {
  V e;
  int f;

  J u()
  {
    for (int x = 0; x < e.s(); ++x)
      if (e[x])
        return J(e[x], this);
    return v();
  }
  J v() { return J((N*)64, this); }
};

struct I {
  H d;
  J u() { return d.u(); }
  J v() { return d.v(); }
};

struct bar {
  virtual ~bar() {}
};

struct E {
  K g;
  E(K x) : g(x) {}
};

struct foo : public bar {
  K h;
  E i;
  foo(const K x, const E &y) : h(x), i(y) {}
};

struct A {
  I *l;
  foo *baz() const;
};

foo *A::baz() const
{
  return new foo(l->u(), E(l->v()));
}

A x;
I i;
foo *f;

int main ()
{
  x.l = &i;
  f = x.baz();
  if (f->h.c != f->i.g.c || f->h.d != f->i.g.d)
    return 1;
  return 0;
}
