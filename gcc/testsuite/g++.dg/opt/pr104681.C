// PR target/104681
// { dg-do compile }
// { dg-options "-O2" }

void bar ();
struct A {
  A (bool) : a(7.0L), b(0) {}
  long double a;
  long b;
};
struct B {
  void foo () { c = bar; }
  A c;
};
struct C {
  void baz ();
  B d;
};
void C::baz () { d.foo (); }
