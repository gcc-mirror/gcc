// PR c++/93033
// { dg-do compile { target c++11 } }

struct A {
  A ();
  ~A ();
};

A f();

struct B {
  A a;
  bool b;
};

void
foo ()
{
  B i[] { f() };
}
