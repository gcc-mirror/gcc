// Bug: a virtual function with the same name in an unrelated class will
// cause a bogus overloading warning.
// Special g++ Options: -Woverloaded-virtual
// Build don't link:

struct A {
  virtual void foo ();
};

struct B {
  virtual void bar ();
};

struct C: public A {
  virtual void bar  ();
};
