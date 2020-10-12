// PR c++/95074 - Function found via ADL when it should not.
// { dg-do compile }

namespace N {
  struct S { };
  void f(S);
  void g(S);
}

namespace M {
  void f(int);
  void g(int);
}

void
fn0 ()
{
  N::S s;
  using M::f;
  f (s);
}

void
fn1 ()
{
  N::S s;
  extern void f(char);
  using M::f;
  f (s); // { dg-error "no matching function" }
}

void
fn2 ()
{
  N::S s;
  using M::f;
  extern void f(char);
  f (s); // { dg-error "no matching function" }
}

void
fn3 ()
{
  N::S s;
  extern void (*g)(char);
  g (s); // { dg-error "cannot convert" }
}
