// PR c++/93922
// { dg-do link { target c++11 } }

template <typename T>
struct A {
  A () {}
  template <typename U>
  A (A<U> const &) {}
  ~A () {}
};
int t;
struct B {};
struct C : B { C (B const &) { if (t) throw 1; } };
struct S { A<B const> x; C y; };

A<B>
bar (B *)
{
  return A<B> ();
}

S *
foo (B *x, B const &y)
{
  return new S {bar (x), y};
}

int
main ()
{
}
