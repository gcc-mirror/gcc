// { dg-do assemble  }
// { dg-options "-Woverloaded-virtual" }
// Bug: a virtual function with the same name in an unrelated class will
// cause a bogus overloading warning.

struct A {
  virtual void foo ();
};

struct B {
  virtual void bar ();
};

struct C: public A {
  virtual void bar  ();
};
