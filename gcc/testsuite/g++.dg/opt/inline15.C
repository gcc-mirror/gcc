// PR tree-optimization/40813
// { dg-do compile }
// { dg-options "-O -fcheck-new" }

typedef __SIZE_TYPE__ size_t;
typedef void *P;
struct A;
struct B
{
  void *b[5];
  A *foo () { return (A *) & b[0]; }
};
struct A
{
  void *operator new (size_t x, B &y) { return y.foo (); }
};
struct C : public A
{
  virtual int bar () { return 0; }
};
struct D : public C
{
  static B baz (unsigned *x) { B b; new (b) D (x); return b; }
  D (unsigned *x) { }
};
struct E
{
  B e;
  B fn (unsigned *a) { return D::baz (a); }
  E (P b, unsigned *a) : e (fn (a)) { }
};

static unsigned *
fn2 ()
{
  return 0;
}

void
test (P x)
{
  E (x, fn2 ());
}
