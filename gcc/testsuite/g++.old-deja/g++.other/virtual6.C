// { dg-do assemble  }
// { dg-options "-Woverloaded-virtual" }
// Bug: g++ checks certain non-virtual functions to see if they override
// virtual functions.
// Submitted by Jason Merrill <jason@cygnus.com>

struct A {
  virtual void f (int);
};

struct B: public A {
  static void f ();
  void f (int);
};
